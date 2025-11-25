/*
  schema_gen - v0.1 - public domain schema compiler
  
  USAGE:
    schema_gen <input.hischema> <output.h>

  FEATURES:
    - Generates C99 compliant header-only serialization code.
    - No runtime dependencies (only stdint.h, stdbool.h, string.h, math.h).
    - Two encoding modes:
      1. Standard: Tag-Length-Value format (similar to Protobuf but not compatible).
      2. Compact: Highly compact, schema-dependent format (No tags).
    - Tagged unions: first-class `union Name : u16 { Type variant = 1; ... }` that emit discriminator enums + C unions with encode/decode/skip.
    - Automatic endianness handling (encodes to Little Endian).

  COMPARISON:
    - vs Protobuf: 
      - schema_gen produces simple C structs, not complex accessor objects.
      - No large runtime library to link against.
      - "Standard" encoding is similar but uses 2 bits for wire type (vs 3 in PB).
      - Supports a "Compact" mode for maximum bandwidth efficiency.
    - vs FlatBuffers:
      - schema_gen uses a stream format (unpacking), not a table format (pointer chasing).
      - Easier to use for simple network packets where random access isn't needed.

  ENCODING DETAILS:
  
  1. Standard Encoding (Tag-Value)
     [Tag][Value]...
     Tag = (FieldID << 2) | wireType
     wireTypes: 0=Varint, 1=Fixed32, 2=Fixed64, 3=LengthDelimited
  
  2. Compact Encoding
     Designed for minimal size. Fields are written in schema order.
     No tags are written. Optional fields use a bitmask at the start of the struct.
     
     Type Encoding:
     - bool:    1 byte (0x00 or 0x01)
     - u8:      1 byte
     - u16/u32: Varint (1-5 bytes)
     - u64:     Varint (1-10 bytes)
     - i32/i64: ZigZag encoded Varint
     - f32/f64: Compact Float
                [Flag (1 byte)] + [Raw Bytes (4/8 bytes, optional)]
                Flag: 0 = Value is 0.0 (Raw bytes omitted)
                      1 = Value is non-zero (Raw bytes follow)
     - Struct:  Recursively encoded fields in order.
     - Array:   [Count (Varint)] + [Item 0] + [Item 1] ...
     
     Bitmask for Optionals:
     If a struct has optional fields (marked with ?), a bitmask is written first.
     The bitmask is N bytes, where N = ceil(num_optionals / 8).
     Bit 0 corresponds to the first optional field, Bit 1 to the second, etc.
     If the bit is set, the field follows in the stream. If 0, it is skipped.

  LICENSE:
    This software is in the public domain. Where that dedication is not
    recognized, you are granted a perpetual, irrevocable license to copy,
    distribute, and modify this file as you see fit.
*/
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define ARRAY_GROW_CAP(cap) ((cap) ? ((cap) * 2) : 8)

// MARK: Utility helpers

static void fatal(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fputc('\n', stderr);
    exit(EXIT_FAILURE);
}

typedef struct {
    char *data;
    size_t count;
    size_t capacity;
} StringBuilder;

static void sb_init(StringBuilder *sb) {
    sb->data = NULL;
    sb->count = 0;
    sb->capacity = 0;
}

static void sb_reserve(StringBuilder *sb, size_t need) {
    if (need <= sb->capacity) { return; }
    size_t cap = sb->capacity ? sb->capacity : 256;
    while (cap < need) { cap *= 2; }
    char *new_data = (char *)realloc(sb->data, cap);
    if (!new_data) { fatal("schema_gen: out of memory while reserving string builder buffer"); }
    sb->data = new_data;
    sb->capacity = cap;
}

static void sb_append_bytes(StringBuilder *sb, const char *data, size_t len) {
    size_t need = sb->count + len + 1;
    if (need > sb->capacity) { sb_reserve(sb, need); }
    memcpy(sb->data + sb->count, data, len);
    sb->count += len;
    sb->data[sb->count] = '\0';
}

static void sb_append(StringBuilder *sb, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char stack_buf[512];
    int needed = vsnprintf(stack_buf, sizeof(stack_buf), fmt, args);
    va_end(args);
    if (needed < 0) { fatal("schema_gen: vsnprintf failed"); }
    if ((size_t)needed < sizeof(stack_buf)) {
        sb_append_bytes(sb, stack_buf, (size_t)needed);
        return;
    }
    char *heap_buf = (char *)malloc((size_t)needed + 1);
    if (!heap_buf) { fatal("schema_gen: out of memory while formatting text"); }
    va_list args2;
    va_start(args2, fmt);
    vsnprintf(heap_buf, (size_t)needed + 1, fmt, args2);
    va_end(args2);
    sb_append_bytes(sb, heap_buf, (size_t)needed);
    free(heap_buf);
}

static char *read_entire_file(const char *path, size_t *out_size) {
    FILE *f = fopen(path, "rb");
    if (!f) { fatal("schema_gen: failed to open '%s'", path); }
    if (fseek(f, 0, SEEK_END) != 0) { fatal("schema_gen: failed to seek '%s'", path); }
    long len = ftell(f);
    if (len < 0) { fatal("schema_gen: failed to stat '%s'", path); }
    if (fseek(f, 0, SEEK_SET) != 0) { fatal("schema_gen: failed to rewind '%s'", path); }
    char *buffer = (char *)malloc((size_t)len + 1);
    if (!buffer) { fatal("schema_gen: out of memory while reading '%s'", path); }
    size_t read_bytes = fread(buffer, 1, (size_t)len, f);
    if (read_bytes != (size_t)len) { fatal("schema_gen: failed to read '%s'", path); }
    buffer[len] = '\0';
    fclose(f);
    if (out_size) { *out_size = (size_t)len; }
    return buffer;
}

static char *str_dup_range(const char *start, const char *end) {
    size_t len = (size_t)(end - start);
    char *result = (char *)malloc(len + 1);
    if (!result) { fatal("schema_gen: out of memory while duplicating text"); }
    memcpy(result, start, len);
    result[len] = '\0';
    return result;
}

static char *str_dup(const char *s) {
    size_t len = strlen(s);
    char *result = (char *)malloc(len + 1);
    if (!result) { fatal("schema_gen: out of memory while duplicating string"); }
    memcpy(result, s, len + 1);
    return result;
}

static void to_lower_first(char *dst, size_t dst_size, const char *src) {
    if (!dst_size) { return; }
    size_t len = strlen(src);
    if (len >= dst_size) { len = dst_size - 1; }
    memcpy(dst, src, len);
    dst[len] = '\0';
    if (len > 0) { dst[0] = (char)tolower((unsigned char)dst[0]); }
}

static void to_upper_str(char *dst, size_t dst_size, const char *src) {
    if (!dst_size) { return; }
    size_t len = strlen(src);
    if (len >= dst_size) { len = dst_size - 1; }
    for (size_t i = 0; i < len; ++i) {
        dst[i] = (char)toupper((unsigned char)src[i]);
    }
    dst[len] = '\0';
}

// MARK: Lexing

typedef enum {
    TOKEN_EOF,
    TOKEN_IDENTIFIER,
    TOKEN_NUMBER,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_COLON,
    TOKEN_SEMICOLON,
    TOKEN_COMMA,
    TOKEN_ASSIGN,
    TOKEN_DOT,
    TOKEN_QUESTION,
    TOKEN_LESS,
    TOKEN_GREATER
} TokenKind;

typedef struct {
    TokenKind kind;
    const char *start;
    const char *end;
    size_t line;
} Token;

typedef struct {
    const char *cursor;
    const char *end;
    size_t line;
    const char *filename;
} Lexer;

static void lexer_init(Lexer *lx, const char *input, const char *filename) {
    lx->cursor = input;
    lx->end = input + strlen(input);
    lx->line = 1;
    lx->filename = filename;
}

static void lexer_skip_ws(Lexer *lx) {
    while (lx->cursor < lx->end) {
        char c = *lx->cursor;
        if (c == '\n') {
            lx->line++;
            lx->cursor++;
            continue;
        }
        if (isspace((unsigned char)c)) {
            lx->cursor++;
            continue;
        }
        if (c == '/' && (lx->cursor + 1) < lx->end && lx->cursor[1] == '/') {
            lx->cursor += 2;
            while (lx->cursor < lx->end && *lx->cursor != '\n') { lx->cursor++; }
            continue;
        }
        break;
    }
}

static Token lexer_next(Lexer *lx) {
    lexer_skip_ws(lx);
    Token token;
    token.start = lx->cursor;
    token.end = lx->cursor;
    token.line = lx->line;
    if (lx->cursor >= lx->end) {
        token.kind = TOKEN_EOF;
        return token;
    }
    char c = *lx->cursor++;
    token.start = lx->cursor - 1;
    token.end = lx->cursor;
    switch (c) {
        case '{': token.kind = TOKEN_LBRACE; return token;
        case '}': token.kind = TOKEN_RBRACE; return token;
        case '[': token.kind = TOKEN_LBRACKET; return token;
        case ']': token.kind = TOKEN_RBRACKET; return token;
        case '(': token.kind = TOKEN_LPAREN; return token;
        case ')': token.kind = TOKEN_RPAREN; return token;
        case ':': token.kind = TOKEN_COLON; return token;
        case ';': token.kind = TOKEN_SEMICOLON; return token;
        case ',': token.kind = TOKEN_COMMA; return token;
        case '=': token.kind = TOKEN_ASSIGN; return token;
        case '.': token.kind = TOKEN_DOT; return token;
        case '?': token.kind = TOKEN_QUESTION; return token;
        case '<': token.kind = TOKEN_LESS; return token;
        case '>': token.kind = TOKEN_GREATER; return token;
        default: break;
    }
    if (isalpha((unsigned char)c) || c == '_') {
        while (lx->cursor < lx->end) {
            char nc = *lx->cursor;
            if (!isalnum((unsigned char)nc) && nc != '_') { break; }
            lx->cursor++;
        }
        token.end = lx->cursor;
        token.kind = TOKEN_IDENTIFIER;
        return token;
    }
    if (isdigit((unsigned char)c)) {
        while (lx->cursor < lx->end && isdigit((unsigned char)*lx->cursor)) { lx->cursor++; }
        token.end = lx->cursor;
        token.kind = TOKEN_NUMBER;
        return token;
    }
    fatal("%s:%zu: error: unexpected character '%c'", lx->filename, token.line, c);
    return token;
}

typedef struct {
    Lexer lexer;
    Token current;
    bool has_current;
    char *error;
} Parser;

static void parser_init(Parser *p, const char *input, const char *filename) {
    lexer_init(&p->lexer, input, filename);
    p->has_current = false;
    p->error = NULL;
}

static Token parser_peek(Parser *p) {
    if (!p->has_current) {
        p->current = lexer_next(&p->lexer);
        p->has_current = true;
    }
    return p->current;
}

static Token parser_next(Parser *p) {
    Token tok = parser_peek(p);
    p->has_current = false;
    return tok;
}

static bool parser_match(Parser *p, TokenKind kind) {
    Token tok = parser_peek(p);
    if (tok.kind != kind) { return false; }
    parser_next(p);
    return true;
}

static void parser_expect(Parser *p, TokenKind kind, const char *message) {
    Token tok = parser_next(p);
    if (tok.kind != kind) {
        fatal("%s:%zu: error: %s", p->lexer.filename, tok.line, message);
    }
}

static char *token_to_string(const Token *tok) {
    return str_dup_range(tok->start, tok->end);
}

// MARK: Schema model

typedef struct {
    char *name;
    char *c_type;
} TypeAlias;

typedef struct {
    char *name;
    uint64_t value;
} EnumValue;

typedef struct {
    char *name;
    char *base_type;
    bool is_bitset;
    EnumValue *values;
    size_t value_count;
    size_t value_capacity;
    size_t line;
} EnumDef;

typedef enum {
    STRUCT_KIND_STRUCT,
    STRUCT_KIND_MESSAGE
} StructKind;

typedef enum {
    FIELD_DEFAULT_NONE,
    FIELD_DEFAULT_NUMBER,
    FIELD_DEFAULT_ENUM_VALUE
} FieldDefaultKind;

typedef struct {
    FieldDefaultKind kind;
    int64_t number_value;
    char *enum_value;
} FieldDefault;

typedef struct {
    char *name;
    char *type_name;
    bool is_array;
    bool has_field_id;
    uint32_t field_id;
    FieldDefault def;
    size_t line;
    uint32_t bit_width;
    bool is_optional;
    bool is_deprecated;
    bool is_removed;
    bool is_map;
    char *key_type;
} FieldDef;

typedef struct {
    char *name;
    char *type_name;
    uint32_t field_id;
    size_t line;
} UnionField;

typedef struct {
    char *name;
    char *base_type;
    UnionField *fields;
    size_t field_count;
    size_t field_capacity;
    size_t line;
} UnionDef;

// MARK: Compatibility helpers

typedef struct {
    char *name;
    uint32_t id;
    uint32_t type_id;
    bool is_array;
    bool is_optional;
    bool is_deprecated;
    bool is_removed;
    uint32_t bit_width;
} CompatField;

typedef struct {
    char *name;
    uint8_t kind; // 0=ENUM, 1=STRUCT, 2=MESSAGE
    uint32_t type_id;
    uint32_t union_base_type_id;
    CompatField *fields;
    uint32_t field_count;
} CompatType;

typedef struct {
    CompatType *types;
    size_t type_count;
} CompatSchema;

typedef struct {
    char *name;
    StructKind kind;
    FieldDef *fields;
    size_t field_count;
    size_t field_capacity;
    FieldDef *removed_fields;
    size_t removed_count;
    size_t removed_capacity;
    size_t line;
} StructDef;

typedef struct {
    char *element_type;
    size_t line;
} ArrayType;

typedef struct {
    char *prefix;
    const char *filename;
    TypeAlias *aliases;
    size_t alias_count;
    size_t alias_capacity;
    size_t builtin_alias_count;
    EnumDef *enums;
    size_t enum_count;
    size_t enum_capacity;
    StructDef *structs;
    size_t struct_count;
    size_t struct_capacity;
    UnionDef *unions;
    size_t union_count;
    size_t union_capacity;
    ArrayType *arrays;
    size_t array_count;
    size_t array_capacity;
    bool string_alias_overridden;
} Schema;

static void append_runtime_schema_defs(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro, const StringBuilder *bin_data);

static const struct { const char *name; const char *ctype; } BUILTIN_ALIASES[] = {
    {"u8", "uint8_t"}, {"u16", "uint16_t"}, {"u32", "uint32_t"}, {"u64", "uint64_t"},
    {"i8", "int8_t"}, {"i16", "int16_t"}, {"i32", "int32_t"}, {"i64", "int64_t"},
    {"f32", "float"}, {"f64", "double"}, {"bool", "bool"}, {"string", NULL}
};

static void make_string_typename(char *dst, size_t dst_size, const Schema *schema);
static bool union_base_supported(const char *name);

static void schema_init(Schema *schema) {
    memset(schema, 0, sizeof(*schema));
    schema->prefix = str_dup("gen_");
    schema->string_alias_overridden = false;
    schema->builtin_alias_count = sizeof(BUILTIN_ALIASES) / sizeof(BUILTIN_ALIASES[0]);
    for (size_t i = 0; i < schema->builtin_alias_count; ++i) {
        if (schema->alias_count == schema->alias_capacity) {
            size_t new_cap = ARRAY_GROW_CAP(schema->alias_capacity);
            schema->aliases = (TypeAlias *)realloc(schema->aliases, new_cap * sizeof(TypeAlias));
            if (!schema->aliases) { fatal("schema_gen: out of memory while growing aliases"); }
            schema->alias_capacity = new_cap;
        }
        TypeAlias *alias = &schema->aliases[schema->alias_count++];
        alias->name = str_dup(BUILTIN_ALIASES[i].name);
        if (BUILTIN_ALIASES[i].ctype) {
            alias->c_type = str_dup(BUILTIN_ALIASES[i].ctype);
        } else if (strcmp(BUILTIN_ALIASES[i].name, "string") == 0) {
            char buf[128];
            make_string_typename(buf, sizeof(buf), schema);
            alias->c_type = str_dup(buf);
        }
    }
}

static TypeAlias *schema_find_alias(Schema *schema, const char *name) {
    for (size_t i = 0; i < schema->alias_count; ++i) {
        if (strcmp(schema->aliases[i].name, name) == 0) { return &schema->aliases[i]; }
    }
    return NULL;
}

static ArrayType *schema_find_array(Schema *schema, const char *element_type) {
    for (size_t i = 0; i < schema->array_count; ++i) {
        if (strcmp(schema->arrays[i].element_type, element_type) == 0) { return &schema->arrays[i]; }
    }
    return NULL;
}

static void schema_add_array(Schema *schema, const char *element_type, size_t line) {
    if (schema_find_array(schema, element_type)) { return; }
    if (schema->array_count == schema->array_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(schema->array_capacity);
        schema->arrays = (ArrayType *)realloc(schema->arrays, new_cap * sizeof(ArrayType));
        if (!schema->arrays) { fatal("schema_gen: out of memory while growing arrays"); }
        schema->array_capacity = new_cap;
    }
    schema->arrays[schema->array_count].element_type = str_dup(element_type);
    schema->arrays[schema->array_count].line = line;
    schema->array_count++;
}

static void ensure_unique_field_id(const Schema *schema, const StructDef *st, uint32_t field_id, size_t line, const char *name) {
    for (size_t i = 0; i < st->field_count; ++i) {
        if (st->fields[i].has_field_id && st->fields[i].field_id == field_id) {
            fatal("%s:%zu: error: duplicate field id %u in %s '%s' (field '%s' conflicts with '%s' at line %zu)",
                  schema && schema->filename ? schema->filename : "(unknown file)", line, field_id,
                  st->kind == STRUCT_KIND_MESSAGE ? "message" : "struct",
                  st->name, name, st->fields[i].name, st->fields[i].line);
        }
    }
    for (size_t i = 0; i < st->removed_count; ++i) {
        if (st->removed_fields[i].has_field_id && st->removed_fields[i].field_id == field_id) {
            fatal("%s:%zu: error: field id %u in %s '%s' for '%s' conflicts with removed id declared at line %zu",
                  schema && schema->filename ? schema->filename : "(unknown file)", line, field_id,
                  st->kind == STRUCT_KIND_MESSAGE ? "message" : "struct",
                  st->name, name, st->removed_fields[i].line);
        }
    }
}

static void schema_add_removed_field(const Schema *schema, StructDef *st, FieldDef field) {
    if (!field.has_field_id) {
        fatal("%s:%zu: error: removed field '%s' must specify an explicit id", schema && schema->filename ? schema->filename : "(unknown file)", field.line, field.name);
    }
    ensure_unique_field_id(schema, st, field.field_id, field.line, field.name);
    if (st->removed_count == st->removed_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(st->removed_capacity);
        st->removed_fields = (FieldDef *)realloc(st->removed_fields, new_cap * sizeof(FieldDef));
        if (!st->removed_fields) { fatal("schema_gen: out of memory while growing removed fields"); }
        st->removed_capacity = new_cap;
    }
    st->removed_fields[st->removed_count++] = field;
}

static void schema_add_field(const Schema *schema, StructDef *st, FieldDef field) {
    for (size_t i = 0; i < st->field_count; ++i) {
        if (strcmp(st->fields[i].name, field.name) == 0) {
            fatal("%s:%zu: error: duplicate field '%s' in %s '%s' (previous at line %zu)",
                  schema && schema->filename ? schema->filename : "(unknown file)", field.line, field.name,
                  st->kind == STRUCT_KIND_MESSAGE ? "message" : "struct",
                  st->name, st->fields[i].line);
        }
    }
    if (st->kind == STRUCT_KIND_MESSAGE) {
        if (!field.has_field_id) {
            fatal("%s:%zu: error: missing field id for message field '%s'", schema && schema->filename ? schema->filename : "(unknown file)", field.line, field.name);
        }
        ensure_unique_field_id(schema, st, field.field_id, field.line, field.name);
    } else {
        field.field_id = (uint32_t)st->field_count;
        field.has_field_id = true;
    }
    if (st->field_count == st->field_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(st->field_capacity);
        st->fields = (FieldDef *)realloc(st->fields, new_cap * sizeof(FieldDef));
        if (!st->fields) { fatal("schema_gen: out of memory while growing struct fields"); }
        st->field_capacity = new_cap;
    }
    st->fields[st->field_count++] = field;
}

static const FieldDef *struct_find_field_by_id(const StructDef *st, uint32_t field_id) {
    for (size_t i = 0; i < st->field_count; ++i) {
        if (st->fields[i].has_field_id && st->fields[i].field_id == field_id) {
            return &st->fields[i];
        }
    }
    return NULL;
}

static const FieldDef *struct_find_field_by_name(const StructDef *st, const char *name) {
    for (size_t i = 0; i < st->field_count; ++i) {
        if (strcmp(st->fields[i].name, name) == 0) {
            return &st->fields[i];
        }
    }
    return NULL;
}

static bool struct_has_removed_id(const StructDef *st, uint32_t field_id) {
    for (size_t i = 0; i < st->removed_count; ++i) {
        if (st->removed_fields[i].has_field_id && st->removed_fields[i].field_id == field_id) {
            return true;
        }
    }
    return false;
}

static FieldDefault field_default_none(void) {
    FieldDefault def;
    def.kind = FIELD_DEFAULT_NONE;
    def.number_value = 0;
    def.enum_value = NULL;
    return def;
}

static FieldDefault __attribute__((unused)) field_default_number(int64_t value) {
    FieldDefault def;
    def.kind = FIELD_DEFAULT_NUMBER;
    def.number_value = value;
    def.enum_value = NULL;
    return def;
}

static FieldDefault __attribute__((unused)) field_default_enum(const char *value) {
    FieldDefault def;
    def.kind = FIELD_DEFAULT_ENUM_VALUE;
    def.number_value = 0;
    def.enum_value = str_dup(value);
    return def;
}

static uint64_t token_to_uint64(const Token *tok) {
    return strtoull(tok->start, NULL, 10);
}

static uint32_t bitset_base_bits(const char *base_type) {
    if (strcmp(base_type, "u8") == 0) { return 8; }
    if (strcmp(base_type, "u16") == 0) { return 16; }
    if (strcmp(base_type, "u32") == 0) { return 32; }
    if (strcmp(base_type, "u64") == 0) { return 64; }
    return 0;
}

static bool file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

static bool token_equals(const Token *tok, const char *text) {
    size_t len = (size_t)(tok->end - tok->start);
    return strlen(text) == len && strncmp(tok->start, text, len) == 0;
}

static void make_string_typename(char *dst, size_t dst_size, const Schema *schema) {
    snprintf(dst, dst_size, "%sstr8", schema->prefix);
}

// MARK: Schema parsing

static void parse_prefix(Parser *p, Schema *schema) {
    Token name = parser_next(p);
    if (name.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier after prefix keyword", p->lexer.filename, name.line); }
    free(schema->prefix);
    schema->prefix = token_to_string(&name);
    if (!schema->string_alias_overridden) {
        TypeAlias *alias = schema_find_alias(schema, "string");
        if (alias) {
            free(alias->c_type);
            char buf[128];
            make_string_typename(buf, sizeof(buf), schema);
            alias->c_type = str_dup(buf);
        }
    }
}

static void parse_gentype(Parser *p, Schema *schema) {
    Token name_tok = parser_next(p);
    if (name_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier after gentype keyword", p->lexer.filename, name_tok.line); }
    Token ctype_tok = parser_next(p);
    if (ctype_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier for type mapping", p->lexer.filename, ctype_tok.line); }
    char *name = token_to_string(&name_tok);
    TypeAlias *existing = schema_find_alias(schema, name);
    if (existing) {
        free(existing->c_type);
        existing->c_type = token_to_string(&ctype_tok);
        if (strcmp(name, "string") == 0) { schema->string_alias_overridden = true; }
        free(name);
        return;
    }
    if (schema->alias_count == schema->alias_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(schema->alias_capacity);
        schema->aliases = (TypeAlias *)realloc(schema->aliases, new_cap * sizeof(TypeAlias));
        if (!schema->aliases) { fatal("schema_gen: out of memory while growing aliases"); }
        schema->alias_capacity = new_cap;
    }
    TypeAlias *alias_slot = &schema->aliases[schema->alias_count++];
    alias_slot->name = name;
    alias_slot->c_type = token_to_string(&ctype_tok);
    if (strcmp(name, "string") == 0) { schema->string_alias_overridden = true; }
}

static void parse_enum(Parser *p, Schema *schema, bool is_bitset) {
    Token name_tok = parser_next(p);
    if (name_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier after enum keyword", p->lexer.filename, name_tok.line); }
    char *name = token_to_string(&name_tok);
    char *base_type = str_dup("u32");
    if (parser_match(p, TOKEN_COLON)) {
        Token base = parser_next(p);
        if (base.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier for enum base type", p->lexer.filename, base.line); }
        free(base_type);
        base_type = token_to_string(&base);
    }
    if (is_bitset) {
        if (bitset_base_bits(base_type) == 0) {
            fatal("%s:%zu: error: bitset base type must be one of u8/u16/u32/u64", p->lexer.filename, name_tok.line);
        }
    }
    parser_expect(p, TOKEN_LBRACE, "expected '{' to begin enum");
    EnumDef def = {0};
    def.name = name;
    def.base_type = base_type;
    def.line = name_tok.line;
    def.is_bitset = is_bitset;
    uint64_t last_value = 0;
    bool has_last = false;
    uint32_t bit_index = 0;
    uint32_t bit_width = is_bitset ? bitset_base_bits(base_type) : 0;
    uint64_t used_bits = 0;
    while (!parser_match(p, TOKEN_RBRACE)) {
        Token ident = parser_next(p);
        if (ident.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier in enum", p->lexer.filename, ident.line); }
        uint64_t value = 0;
        bool has_assign = parser_match(p, TOKEN_ASSIGN);
        if (has_assign) {
            Token num = parser_next(p);
            if (num.kind != TOKEN_NUMBER) { fatal("%s:%zu: error: expected number after '=' in enum", p->lexer.filename, num.line); }
            value = token_to_uint64(&num);
        } else if (is_bitset) {
            if (bit_width > 0 && bit_index >= bit_width) {
                fatal("%s:%zu: error: too many flags in bitset '%s' for base type %s (max %u bits)",
                      p->lexer.filename, ident.line, name, base_type, bit_width);
            }
            value = UINT64_C(1) << bit_index;
        } else {
            value = has_last ? (last_value + 1) : 0;
        }
        if (is_bitset) {
            if (bit_width < 64) {
                uint64_t max_value = (UINT64_C(1) << bit_width);
                if (value >= max_value) {
                    fatal("%s:%zu: error: value %llu overflows bitset base type %s",
                          p->lexer.filename, ident.line, (unsigned long long)value, base_type);
                }
            }
            if (value == 0 || (value & (value - 1)) != 0) {
                fatal("%s:%zu: error: bitset flag '%.*s' must be a single bit value",
                      p->lexer.filename, ident.line, (int)(ident.end - ident.start), ident.start);
            }
            if (value & used_bits) {
                fatal("%s:%zu: error: duplicate bit value for flag '%.*s' in bitset '%s'",
                      p->lexer.filename, ident.line, (int)(ident.end - ident.start), ident.start, name);
            }
            used_bits |= value;
            bit_index++;
        } else {
            has_last = true;
            last_value = value;
        }
        if (def.value_count == def.value_capacity) {
            size_t new_cap = ARRAY_GROW_CAP(def.value_capacity);
            def.values = (EnumValue *)realloc(def.values, new_cap * sizeof(EnumValue));
            if (!def.values) { fatal("schema_gen: out of memory while growing enum members"); }
            def.value_capacity = new_cap;
        }
        def.values[def.value_count].name = token_to_string(&ident);
        def.values[def.value_count].value = value;
        def.value_count++;
        parser_match(p, TOKEN_COMMA);
    }
    if (schema->enum_count == schema->enum_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(schema->enum_capacity);
        schema->enums = (EnumDef *)realloc(schema->enums, new_cap * sizeof(EnumDef));
        if (!schema->enums) { fatal("schema_gen: out of memory while growing enums"); }
        schema->enum_capacity = new_cap;
    }
    schema->enums[schema->enum_count++] = def;
}

static void parse_struct_field(Parser *p, Schema *schema, StructDef *def, FieldDef *out_field, bool *out_is_removed) {
    bool is_deprecated = false;
    bool is_removed_attr = false;
    bool attr_has_id = false;
    size_t attr_line = 0;
    uint32_t attr_id = 0;
    while (parser_match(p, TOKEN_LBRACKET)) {
        Token attr = parser_next(p);
        if (attr.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected attribute identifier", p->lexer.filename, attr.line); }
        if (token_equals(&attr, "deprecated")) {
            is_deprecated = true;
        } else if (token_equals(&attr, "removed")) {
            is_removed_attr = true;
            attr_line = attr.line;
        } else if (token_equals(&attr, "reserved")) {
            fatal("%s:%zu: error: 'reserved' attribute is no longer supported; use [removed] with an explicit id instead", p->lexer.filename, attr.line);
        } else {
            fatal("%s:%zu: error: unknown attribute '%.*s'", p->lexer.filename, attr.line, (int)(attr.end - attr.start), attr.start);
        }
        parser_expect(p, TOKEN_RBRACKET, "expected ']' after attribute");
    }

    if (is_removed_attr && def->kind != STRUCT_KIND_MESSAGE) {
        size_t line = attr_line ? attr_line : p->lexer.line;
        fatal("%s:%zu: error: removed fields are only valid inside messages", p->lexer.filename, line);
    }

    Token type_tok = parser_next(p);
    if (type_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier in struct body", p->lexer.filename, type_tok.line); }
    
    bool is_map = false;
    char *key_type = NULL;
    char *value_type = NULL;
    if (token_equals(&type_tok, "map")) {
        is_map = true;
        parser_expect(p, TOKEN_LESS, "expected '<' after map");
        Token key_tok = parser_next(p);
        if (key_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected key type identifier", p->lexer.filename, key_tok.line); }
        parser_expect(p, TOKEN_COMMA, "expected ',' after key type");
        Token value_tok = parser_next(p);
        if (value_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected value type identifier", p->lexer.filename, value_tok.line); }
        parser_expect(p, TOKEN_GREATER, "expected '>' after value type");

        key_type = token_to_string(&key_tok);
        value_type = token_to_string(&value_tok);
        // Synthesize Pair struct used to encode map entries
        char pair_name[256];
        char key_title[256];
        snprintf(key_title, sizeof(key_title), "%s", key_type);
        if (key_title[0]) key_title[0] = toupper((unsigned char)key_title[0]);

        char val_title[256];
        snprintf(val_title, sizeof(val_title), "%s", value_type);
        if (val_title[0]) val_title[0] = toupper((unsigned char)val_title[0]);

        snprintf(pair_name, sizeof(pair_name), "%s%sPair", key_title, val_title);

        bool exists = false;
        for (size_t i = 0; i < schema->struct_count; ++i) {
            if (strcmp(schema->structs[i].name, pair_name) == 0) {
                exists = true;
                break;
            }
        }
        
        if (!exists) {
            StructDef pair_def = {0};
            pair_def.name = str_dup(pair_name);
            pair_def.kind = STRUCT_KIND_STRUCT;
            pair_def.line = type_tok.line;

            FieldDef key_field = {0};
            key_field.name = str_dup("key");
            key_field.type_name = key_type;
            key_field.line = type_tok.line;
            key_field.has_field_id = true;
            key_field.field_id = 0;
            key_field.def = field_default_none();
            schema_add_field(schema, &pair_def, key_field);
            
            FieldDef val_field = {0};
            val_field.name = str_dup("value");
            val_field.type_name = value_type;
            val_field.line = type_tok.line;
            val_field.has_field_id = true;
            val_field.field_id = 1;
            val_field.def = field_default_none();
            schema_add_field(schema, &pair_def, val_field);
            
            if (schema->struct_count == schema->struct_capacity) {
                size_t new_cap = ARRAY_GROW_CAP(schema->struct_capacity);
                schema->structs = (StructDef *)realloc(schema->structs, new_cap * sizeof(StructDef));
                if (!schema->structs) { fatal("schema_gen: out of memory while growing structs"); }
                schema->struct_capacity = new_cap;
            }
            schema->structs[schema->struct_count++] = pair_def;
        } else {
            free(value_type);
        }
    }

    bool is_array = false;
    if (parser_match(p, TOKEN_LBRACKET)) {
        if (is_map) { fatal("%s:%zu: error: maps cannot be arrays", p->lexer.filename, type_tok.line); }
        parser_expect(p, TOKEN_RBRACKET, "expected ']' after '['");
        is_array = true;
    }
    Token name_tok = parser_next(p);
    if (name_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected field name", p->lexer.filename, name_tok.line); }
    bool is_optional = false;
    if (parser_match(p, TOKEN_QUESTION)) {
        if (is_map) { fatal("%s:%zu: error: maps cannot be optional", p->lexer.filename, type_tok.line); }
        is_optional = true;
    }

    uint32_t field_id = attr_id;
    bool has_field_id = attr_has_id;
    if (parser_match(p, TOKEN_ASSIGN)) {
        Token value = parser_next(p);
        if (value.kind != TOKEN_NUMBER) {
            fatal("%s:%zu: error: expected numeric field id after '=' (defaults are no longer supported)", p->lexer.filename, value.line);
        }
        uint64_t raw_id = token_to_uint64(&value);
        if (raw_id > UINT32_MAX) {
            fatal("%s:%zu: error: field id exceeds 32-bit range", p->lexer.filename, value.line);
        }
        uint32_t parsed_id = (uint32_t)raw_id;
        if (has_field_id && parsed_id != field_id) {
            fatal("%s:%zu: error: conflicting field id for '%.*s' (reserved(%u) vs = %u)",
                  p->lexer.filename, value.line, (int)(name_tok.end - name_tok.start), name_tok.start, field_id, parsed_id);
        }
        field_id = parsed_id;
        has_field_id = true;
    }

    if (def->kind == STRUCT_KIND_MESSAGE) {
        if (!has_field_id) {
            fatal("%s:%zu: error: message field '%.*s' must declare an explicit numeric id",
                  p->lexer.filename, name_tok.line, (int)(name_tok.end - name_tok.start), name_tok.start);
        }
        if (field_id == 0) {
            fatal("%s:%zu: error: field ids must start at 1 (got 0 for '%.*s')",
                  p->lexer.filename, name_tok.line, (int)(name_tok.end - name_tok.start), name_tok.start);
        }
    } else if (has_field_id) {
        fatal("%s:%zu: error: struct field '%.*s' cannot declare an id (defaults removed)",
              p->lexer.filename, name_tok.line, (int)(name_tok.end - name_tok.start), name_tok.start);
    }

    uint32_t bit_width = 0;
    if (parser_match(p, TOKEN_COLON)) {
        Token width_tok = parser_next(p);
        if (width_tok.kind != TOKEN_NUMBER) { fatal("%s:%zu: error: expected bit width after ':'", p->lexer.filename, width_tok.line); }
        bit_width = (uint32_t)token_to_uint64(&width_tok);
        if (bit_width == 0) { fatal("%s:%zu: error: bit width must be positive", p->lexer.filename, width_tok.line); }
    }
    parser_expect(p, TOKEN_SEMICOLON, "expected ';' after field");
    FieldDef field = {0};
    field.name = token_to_string(&name_tok);
    if (is_map) {
        char key_title[256];
        snprintf(key_title, sizeof(key_title), "%s", key_type);
        if (key_title[0]) key_title[0] = toupper((unsigned char)key_title[0]);
        
        char val_title[256];
        snprintf(val_title, sizeof(val_title), "%s", value_type);
        if (val_title[0]) val_title[0] = toupper((unsigned char)val_title[0]);

        char pair_name[256];
        snprintf(pair_name, sizeof(pair_name), "%s%sPair", key_title, val_title);
        
        field.type_name = str_dup(pair_name);
        field.is_array = true; // Map is encoded as an array of key/value pairs
        field.is_map = true;
        field.key_type = key_type;
    } else {
        field.type_name = token_to_string(&type_tok);
        field.is_array = is_array;
        field.is_map = false;
    }
    
    field.has_field_id = has_field_id;
    field.field_id = field_id;
    field.def = field_default_none();
    field.line = type_tok.line;
    field.bit_width = bit_width;
    field.is_optional = is_optional;
    field.is_deprecated = is_deprecated;
    field.is_removed = is_removed_attr;
    if (field.is_array) {
        if (bit_width > 0) { fatal("%s:%zu: error: arrays cannot have bit width", p->lexer.filename, type_tok.line); }
        schema_add_array(schema, field.type_name, field.line);
    }
    if (field.is_removed && (field.is_optional || field.is_array)) {
        fatal("%s:%zu: error: removed fields cannot be optional or arrays", p->lexer.filename, type_tok.line);
    }
    *out_field = field;
    *out_is_removed = field.is_removed;
}

static void parse_struct(Parser *p, Schema *schema, StructKind kind) {
    Token name_tok = parser_next(p);
    if (name_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier after struct/message keyword", p->lexer.filename, name_tok.line); }
    StructDef def = {0};
    def.name = token_to_string(&name_tok);
    def.kind = kind;
    parser_expect(p, TOKEN_LBRACE, "expected '{' after struct/message name");
    while (!parser_match(p, TOKEN_RBRACE)) {
        FieldDef field = {0};
        bool is_removed = false;
        parse_struct_field(p, schema, &def, &field, &is_removed);
        if (is_removed) {
            schema_add_removed_field(schema, &def, field);
        } else {
            schema_add_field(schema, &def, field);
        }
    }
    def.line = name_tok.line;
    if (schema->struct_count == schema->struct_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(schema->struct_capacity);
        schema->structs = (StructDef *)realloc(schema->structs, new_cap * sizeof(StructDef));
        if (!schema->structs) { fatal("schema_gen: out of memory while growing structs"); }
        schema->struct_capacity = new_cap;
    }
    schema->structs[schema->struct_count++] = def;
}

static void parse_union(Parser *p, Schema *schema) {
    Token name_tok = parser_next(p);
    if (name_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier after union keyword", p->lexer.filename, name_tok.line); }
    char *name = token_to_string(&name_tok);
    char *base_type = str_dup("u32");
    if (parser_match(p, TOKEN_COLON)) {
        Token base_tok = parser_next(p);
        if (base_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected identifier for union tag base type", p->lexer.filename, base_tok.line); }
        free(base_type);
        base_type = token_to_string(&base_tok);
    }
    if (!union_base_supported(base_type)) {
        fatal("%s:%zu: error: union base type must be one of u8/u16/u32/u64", p->lexer.filename, name_tok.line);
    }
    parser_expect(p, TOKEN_LBRACE, "expected '{' after union name");
    UnionDef def = {0};
    def.name = name;
    def.base_type = base_type;
    def.line = name_tok.line;

    while (!parser_match(p, TOKEN_RBRACE)) {
        Token type_tok = parser_next(p);
        if (type_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected type identifier in union", p->lexer.filename, type_tok.line); }
        Token field_tok = parser_next(p);
        if (field_tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: expected variant name in union", p->lexer.filename, field_tok.line); }
        if (parser_match(p, TOKEN_QUESTION)) {
            fatal("%s:%zu: error: union variants cannot be optional", p->lexer.filename, field_tok.line);
        }
        if (parser_match(p, TOKEN_LBRACKET)) {
            fatal("%s:%zu: error: union variants cannot be arrays", p->lexer.filename, field_tok.line);
        }
        if (!parser_match(p, TOKEN_ASSIGN)) {
            fatal("%s:%zu: error: expected '=' with numeric tag id for union variant '%.*s'", p->lexer.filename, field_tok.line, (int)(field_tok.end - field_tok.start), field_tok.start);
        }
        Token id_tok = parser_next(p);
        if (id_tok.kind != TOKEN_NUMBER) {
            fatal("%s:%zu: error: expected numeric tag id for union variant '%.*s'", p->lexer.filename, id_tok.line, (int)(field_tok.end - field_tok.start), field_tok.start);
        }
        uint64_t raw_id = token_to_uint64(&id_tok);
        if (raw_id == 0 || raw_id > UINT32_MAX) {
            fatal("%s:%zu: error: union tag ids must be in 1..UINT32_MAX (got %llu)", p->lexer.filename, id_tok.line, (unsigned long long)raw_id);
        }
        parser_expect(p, TOKEN_SEMICOLON, "expected ';' after union variant");

        UnionField variant = {0};
        variant.name = token_to_string(&field_tok);
        variant.type_name = token_to_string(&type_tok);
        variant.field_id = (uint32_t)raw_id;
        variant.line = type_tok.line;

        for (size_t i = 0; i < def.field_count; ++i) {
            if (strcmp(def.fields[i].name, variant.name) == 0) {
                fatal("%s:%zu: error: duplicate union variant name '%s' (previous at line %zu)", p->lexer.filename, field_tok.line, variant.name, def.fields[i].line);
            }
            if (def.fields[i].field_id == variant.field_id) {
                fatal("%s:%zu: error: duplicate union tag id %u (conflicts with '%s' at line %zu)", p->lexer.filename, id_tok.line, variant.field_id, def.fields[i].name, def.fields[i].line);
            }
        }

        if (def.field_count == def.field_capacity) {
            size_t new_cap = ARRAY_GROW_CAP(def.field_capacity);
            def.fields = (UnionField *)realloc(def.fields, new_cap * sizeof(UnionField));
            if (!def.fields) { fatal("schema_gen: out of memory while growing union variants"); }
            def.field_capacity = new_cap;
        }
        def.fields[def.field_count++] = variant;
    }

    if (schema->union_count == schema->union_capacity) {
        size_t new_cap = ARRAY_GROW_CAP(schema->union_capacity);
        schema->unions = (UnionDef *)realloc(schema->unions, new_cap * sizeof(UnionDef));
        if (!schema->unions) { fatal("schema_gen: out of memory while growing unions"); }
        schema->union_capacity = new_cap;
    }
    schema->unions[schema->union_count++] = def;
}

static void parse_schema(Parser *p, Schema *schema) {
    while (true) {
        Token tok = parser_peek(p);
        if (tok.kind == TOKEN_EOF) { break; }
        if (tok.kind != TOKEN_IDENTIFIER) { fatal("%s:%zu: error: unexpected token while parsing", p->lexer.filename, tok.line); }
        if (token_equals(&tok, "prefix")) {
            parser_next(p);
            parse_prefix(p, schema);
            continue;
        }
        if (token_equals(&tok, "gentype")) {
            parser_next(p);
            parse_gentype(p, schema);
            continue;
        }
        if (token_equals(&tok, "type")) {
            fatal("%s:%zu: error: 'type' has been renamed to 'gentype' (use 'gentype <schema_type> <c_type>')", p->lexer.filename, tok.line);
        }
        if (token_equals(&tok, "enum")) {
            parser_next(p);
            parse_enum(p, schema, false);
            continue;
        }
        if (token_equals(&tok, "bitset")) {
            parser_next(p);
            parse_enum(p, schema, true);
            continue;
        }
        if (token_equals(&tok, "struct")) {
            parser_next(p);
            parse_struct(p, schema, STRUCT_KIND_STRUCT);
            continue;
        }
        if (token_equals(&tok, "message")) {
            parser_next(p);
            parse_struct(p, schema, STRUCT_KIND_MESSAGE);
            continue;
        }
        if (token_equals(&tok, "union")) {
            parser_next(p);
            parse_union(p, schema);
            continue;
        }
        fatal("%s:%zu: error: unsupported top-level token", p->lexer.filename, tok.line);
    }
}

static const EnumDef *find_enum_const(const Schema *schema, const char *name) {
    for (size_t i = 0; i < schema->enum_count; ++i) {
        if (strcmp(schema->enums[i].name, name) == 0) { return &schema->enums[i]; }
    }
    return NULL;
}

static const StructDef *find_struct_const(const Schema *schema, const char *name) {
    for (size_t i = 0; i < schema->struct_count; ++i) {
        if (strcmp(schema->structs[i].name, name) == 0) { return &schema->structs[i]; }
    }
    return NULL;
}

static const UnionDef *find_union_const(const Schema *schema, const char *name) {
    for (size_t i = 0; i < schema->union_count; ++i) {
        if (strcmp(schema->unions[i].name, name) == 0) { return &schema->unions[i]; }
    }
    return NULL;
}

static bool type_is_struct_type(const Schema *schema, const char *name) {
    if (find_struct_const(schema, name)) { return true; }
    if (find_union_const(schema, name)) { return true; }
    return false;
}

static const TypeAlias *find_alias_const(const Schema *schema, const char *name) {
    for (size_t i = 0; i < schema->alias_count; ++i) {
        if (strcmp(schema->aliases[i].name, name) == 0) { return &schema->aliases[i]; }
    }
    return NULL;
}

static bool is_scalar_type(const Schema *schema, const char *type_name) {
    if (find_alias_const(schema, type_name)) { return true; }
    if (find_enum_const(schema, type_name)) { return true; }
    return false;
}

static void make_prefixed(char *dst, size_t dst_size, const Schema *schema, const char *name) {
    snprintf(dst, dst_size, "%s%s", schema->prefix, name);
}

static void make_array_name(char *dst, size_t dst_size, const Schema *schema, const char *element) {
    snprintf(dst, dst_size, "%s%sArray", schema->prefix, element);
}

static void make_enum_value_name(char *dst, size_t dst_size, const Schema *schema, const char *enum_name, const char *value) {
    snprintf(dst, dst_size, "%s%s_%s", schema->prefix, enum_name, value);
}

static void __attribute__((unused)) make_parameter_enum_name(char *dst, size_t dst_size, const Schema *schema, const StructDef *def) {
    char lower[256];
    to_lower_first(lower, sizeof(lower), def->name);
    snprintf(dst, dst_size, "%s%sParameters", schema->prefix, lower);
}

static void make_union_tag_enum_name(char *dst, size_t dst_size, const Schema *schema, const UnionDef *def) {
    char lower[256];
    to_lower_first(lower, sizeof(lower), def->name);
    snprintf(dst, dst_size, "%s%sTag", schema->prefix, lower);
}

static bool type_is_bool(const char *name) { return strcmp(name, "bool") == 0; }
static bool type_is_u8(const char *name) { return strcmp(name, "u8") == 0; }
static bool type_is_u64(const char *name) { return strcmp(name, "u64") == 0; }
static bool type_is_u32_compatible(const char *name) {
    return strcmp(name, "u16") == 0 || strcmp(name, "u32") == 0;
}
static bool type_is_signed32(const char *name) {
    return strcmp(name, "i8") == 0 || strcmp(name, "i16") == 0 || strcmp(name, "i32") == 0;
}
static bool type_is_signed64(const char *name) { return strcmp(name, "i64") == 0; }
static bool type_is_f32(const char *name) { return strcmp(name, "f32") == 0; }
static bool type_is_f64(const char *name) { return strcmp(name, "f64") == 0; }
static bool type_is_string(const char *name) { return strcmp(name, "string") == 0; }

static bool union_base_supported(const char *name) {
    return strcmp(name, "u8") == 0 || strcmp(name, "u16") == 0 || strcmp(name, "u32") == 0 || strcmp(name, "u64") == 0;
}

static const char *c_type_for(const Schema *schema, const char *type_name, char *buffer, size_t buffer_size, size_t line) {
    const TypeAlias *alias = find_alias_const(schema, type_name);
    if (alias) { return alias->c_type; }
    const EnumDef *en = find_enum_const(schema, type_name);
    if (en) { make_prefixed(buffer, buffer_size, schema, en->name); return buffer; }
    const StructDef *st = find_struct_const(schema, type_name);
    if (st) { make_prefixed(buffer, buffer_size, schema, st->name); return buffer; }
    const UnionDef *un = find_union_const(schema, type_name);
    if (un) { make_prefixed(buffer, buffer_size, schema, un->name); return buffer; }
    fatal("%s:%zu: error: unknown type '%s'", schema->filename, line, type_name);
    return NULL;
}

static bool type_is_enum(const Schema *schema, const char *type_name) {
    return find_enum_const(schema, type_name) != NULL;
}

static const char *field_base_type(const Schema *schema, const FieldDef *field) {
    const EnumDef *en = find_enum_const(schema, field->type_name);
    return en ? en->base_type : field->type_name;
}

static bool base_type_is_signed(const char *base_type) {
    return type_is_signed32(base_type) || type_is_signed64(base_type);
}

static bool base_type_is_integer(const char *base_type) {
    return type_is_u8(base_type) || type_is_u32_compatible(base_type) || type_is_u64(base_type) || base_type_is_signed(base_type);
}

// MARK: Compact codec generation

static void validate_bit_width(const Schema *schema, const FieldDef *field, const char *base_type) {
    if (field->bit_width == 0) { return; }
    if (!base_type_is_integer(base_type)) {
        fatal("%s:%zu: error: bit width is only supported for integer fields (field '%s' of type '%s')",
              schema->filename ? schema->filename : "(unknown file)", field->line, field->name, base_type);
    }
    if (field->bit_width > 64) {
        fatal("%s:%zu: error: bit width on field '%s' exceeds 64 bits (got %u)",
              schema->filename ? schema->filename : "(unknown file)", field->line, field->name, field->bit_width);
    }
}

static void append_bit_width_assign(StringBuilder *out, const Schema *schema, const FieldDef *field, const char *source_expr, const char *target_expr, const char *indent) {
    const char *base_type = field_base_type(schema, field);
    validate_bit_width(schema, field, base_type);
    if (field->bit_width == 0) {
        sb_append(out, "%s%s = %s;\n", indent, target_expr, source_expr);
        return;
    }

    char mask_literal[64];
    if (field->bit_width >= 64) {
        snprintf(mask_literal, sizeof(mask_literal), "UINT64_MAX");
    } else {
        snprintf(mask_literal, sizeof(mask_literal), "((UINT64_C(1) << %u) - UINT64_C(1))", field->bit_width);
    }

    char ctype_buf[256];
    const char *ctype = c_type_for(schema, field->type_name, ctype_buf, sizeof(ctype_buf), field->line);
    sb_append(out, "%sconst uint64_t mask = %s;\n", indent, mask_literal);
    sb_append(out, "%suint64_t masked = ((uint64_t)(%s)) & mask;\n", indent, source_expr);
    if (base_type_is_signed(base_type) && field->bit_width < 64) {
        sb_append(out, "%sconst uint64_t sign_bit = UINT64_C(1) << %u;\n", indent, field->bit_width - 1);
        sb_append(out, "%sif (masked & sign_bit) { masked |= ~mask; }\n", indent);
    }
    sb_append(out, "%s%s = (%s)masked;\n", indent, target_expr, ctype);
}

static void append_scalar_encode(StringBuilder *out, const Schema *schema, const FieldDef *field, const char *value_expr, const char *indent) {
    const char *base_type = field_base_type(schema, field);
    validate_bit_width(schema, field, base_type);
    const char *value_ref = value_expr;
    char inner_indent[64];
    const char *emit_indent = indent;
    bool has_bit_width = field->bit_width > 0;
    if (has_bit_width) {
        snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
        char ctype_buf[256];
        const char *ctype = c_type_for(schema, field->type_name, ctype_buf, sizeof(ctype_buf), field->line);
        sb_append(out, "%s{\n", indent);
        sb_append(out, "%s    %s bw_value;\n", indent, ctype);
        append_bit_width_assign(out, schema, field, value_expr, "bw_value", inner_indent);
        value_ref = "bw_value";
        emit_indent = inner_indent;
    }
    const char *p = schema->prefix;
    if (type_is_bool(base_type)) {
        sb_append(out, "%sif (!%sbuffer_write_bool(buffer, %s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_u8(base_type)) {
        sb_append(out, "%sif (!%sbuffer_write_u8(buffer, %s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_u64(base_type)) {
        sb_append(out, "%sif (!%swrite_var_u64(buffer, (uint64_t)%s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_u32_compatible(base_type)) {
        sb_append(out, "%sif (!%swrite_var_u32(buffer, (uint32_t)%s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_signed64(base_type)) {
        sb_append(out, "%sif (!%swrite_var_s64(buffer, (int64_t)%s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_signed32(base_type)) {
        sb_append(out, "%sif (!%swrite_var_s32(buffer, (int32_t)%s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_f32(base_type)) {
        sb_append(out, "%sif (!%swrite_compact_f32(buffer, %s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_f64(base_type)) {
        sb_append(out, "%sif (!%swrite_compact_f64(buffer, %s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_f64(base_type)) {
        sb_append(out, "%sif (!%swrite_compact_f64(buffer, %s)) { return false; }\n", emit_indent, p, value_ref);
    } else if (type_is_string(base_type)) {
        sb_append(out, "%sif (!%sbuffer_write_string(buffer, &%s)) { return false; }\n", emit_indent, p, value_ref);
    } else {
        fatal("schema_gen: unsupported scalar type '%s' for compact encoding", base_type);
    }
    if (has_bit_width) {
        sb_append(out, "%s}\n", indent);
    }
}

static void append_scalar_decode(StringBuilder *out, const Schema *schema, const FieldDef *field, const char *target_expr, const char *indent, const char *failure_action) {
    const char *base_type = field_base_type(schema, field);
    validate_bit_width(schema, field, base_type);
    char inner_indent[64];
    const char *emit_indent = indent;
    const char *target_ref = target_expr;
    bool has_bit_width = field->bit_width > 0;
    char ctype_buf[256];
    const char *ctype = has_bit_width ? c_type_for(schema, field->type_name, ctype_buf, sizeof(ctype_buf), field->line) : NULL;
    if (has_bit_width) {
        snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
        sb_append(out, "%s{\n", indent);
        sb_append(out, "%s%s bw_value;\n", inner_indent, ctype);
        target_ref = "bw_value";
        emit_indent = inner_indent;
    }
    const char *p = schema->prefix;
    const char *fail = failure_action ? failure_action : "return false;";
    if (type_is_bool(base_type)) {
        sb_append(out, "%sif (!%sbuffer_read_bool(buffer, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else if (type_is_u8(base_type)) {
        sb_append(out, "%sif (!%sbuffer_read_u8(buffer, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else if (type_is_u64(base_type)) {
        sb_append(out,
            "%s{\n"
            "%s    uint64_t tmp = 0;\n"
            "%s    if (!%sread_var_u64(buffer, &tmp)) { %s }\n"
            "%s    %s = (uint64_t)tmp;\n"
            "%s}\n",
            emit_indent, emit_indent, emit_indent, p, fail, emit_indent, target_ref, emit_indent);
    } else if (type_is_u32_compatible(base_type)) {
        sb_append(out,
            "%s{\n"
            "%s    uint32_t tmp = 0;\n"
            "%s    if (!%sread_var_u32(buffer, &tmp)) { %s }\n"
            "%s    %s = (uint32_t)tmp;\n"
            "%s}\n",
            emit_indent, emit_indent, emit_indent, p, fail, emit_indent, target_ref, emit_indent);
    } else if (type_is_signed64(base_type)) {
        sb_append(out,
            "%s{\n"
            "%s    int64_t tmp = 0;\n"
            "%s    if (!%sread_var_s64(buffer, &tmp)) { %s }\n"
            "%s    %s = (int64_t)tmp;\n"
            "%s}\n",
            emit_indent, emit_indent, emit_indent, p, fail, emit_indent, target_ref, emit_indent);
    } else if (type_is_signed32(base_type)) {
        sb_append(out,
            "%s{\n"
            "%s    int32_t tmp = 0;\n"
            "%s    if (!%sread_var_s32(buffer, &tmp)) { %s }\n"
            "%s    %s = (int32_t)tmp;\n"
            "%s}\n",
            emit_indent, emit_indent, emit_indent, p, fail, emit_indent, target_ref, emit_indent);
    } else if (type_is_f32(base_type)) {
        sb_append(out, "%sif (!%sread_compact_f32(buffer, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else if (type_is_f64(base_type)) {
        sb_append(out, "%sif (!%sread_compact_f64(buffer, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else if (type_is_f64(base_type)) {
        sb_append(out, "%sif (!%sread_compact_f64(buffer, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else if (type_is_string(base_type)) {
        sb_append(out, "%sif (!%sbuffer_read_string(buffer, memory, &%s)) { %s }\n", emit_indent, p, target_ref, fail);
    } else {
        fatal("schema_gen: unsupported scalar type '%s' for compact decoding", base_type);
    }
    if (has_bit_width) {
        append_bit_width_assign(out, schema, field, "bw_value", target_expr, emit_indent);
        sb_append(out, "%s}\n", indent);
    }
}

static void __attribute__((unused)) wire_kind_literal(char *dst, size_t dst_size, const Schema *schema, const FieldDef *field) {
    if (field->is_array || type_is_struct_type(schema, field->type_name) ||
        type_is_f32(field_base_type(schema, field)) || type_is_f64(field_base_type(schema, field))) {
        snprintf(dst, dst_size, "%swireType_LengthDelimited", schema->prefix);
    } else {
        snprintf(dst, dst_size, "%swireType_Varint", schema->prefix);
    }
}

static bool __attribute__((unused)) field_uses_length_wire(const Schema *schema, const FieldDef *field) {
    if (field->is_array || type_is_struct_type(schema, field->type_name)) { return true; }
    const char *base = field_base_type(schema, field);
    return type_is_f32(base) || type_is_f64(base);
}

static void append_struct_codec(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const StructDef *def, const char *api_macro) {
    char buffer_type[256];
    snprintf(buffer_type, sizeof(buffer_type), "%sBuffer", schema->prefix);
    char struct_name[256];
    make_prefixed(struct_name, sizeof(struct_name), schema, def->name);

    sb_append(decls, "%s bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema);\n", api_macro, struct_name, struct_name, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema);\n", api_macro, struct_name, struct_name, buffer_type, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_skip_compact(%s *buffer);\n\n", api_macro, struct_name, buffer_type);

    sb_append(impl, "bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema) {\n", struct_name, struct_name, buffer_type, schema->prefix);
    sb_append(impl, "    (void)schema;\n");
    
    uint32_t optional_count = 0;
    for (size_t i = 0; i < def->field_count; ++i) {
        if (def->fields[i].is_optional) { optional_count++; }
    }
    bool has_bitfields = false;
    for (size_t i = 0; i < def->field_count; ++i) {
        if (def->fields[i].bit_width > 0) { has_bitfields = true; break; }
    }

    if (optional_count > 0) {
        uint32_t bitmask_size = (optional_count + 7) / 8;
        sb_append(impl, "    uint8_t bitmask[%u];\n", bitmask_size);
        sb_append(impl, "    memset(bitmask, 0, sizeof(bitmask));\n");
        
        uint32_t opt_idx = 0;
        for (size_t i = 0; i < def->field_count; ++i) {
            if (def->fields[i].is_optional) {
                sb_append(impl, "    if (value->%sExist) { bitmask[%u] |= (1u << %u); }\n", def->fields[i].name, opt_idx / 8, opt_idx % 8);
                opt_idx++;
            }
        }
        sb_append(impl, "    if (!%sbuffer_write_bytes(buffer, bitmask, sizeof(bitmask))) { return false; }\n", schema->prefix);
    }
    if (has_bitfields) {
        sb_append(impl, "    %sBitWriter bit_writer = {0, 0};\n", schema->prefix);
    }

    for (size_t i = 0; i < def->field_count; ++i) {
        const FieldDef *field = &def->fields[i];
        bool is_bitfield = field->bit_width > 0;
        
        if (field->is_optional) {
            sb_append(impl, "    if (value->%sExist) {\n", field->name);
        }

        const char *indent = field->is_optional ? "        " : "    ";
        if (has_bitfields && !is_bitfield) {
            sb_append(impl, "%sif (!%sbitwriter_flush(&bit_writer, buffer)) { return false; }\n", indent, schema->prefix);
        }

        char field_expr[256];
        snprintf(field_expr, sizeof(field_expr), "value->%s", field->name);
        if (field->is_array) {
            sb_append(impl, "%sif (!%swrite_var_u32(buffer, value->%s.count)) { return false; }\n", indent, schema->prefix, field->name);
            sb_append(impl, "%sfor (uint32_t i = 0; i < value->%s.count; ++i) {\n", indent, field->name);
            if (type_is_struct_type(schema, field->type_name)) {
                char element_struct[256];
                make_prefixed(element_struct, sizeof(element_struct), schema, field->type_name);
                sb_append(impl, "%s    if (!%s_encode_compact(&value->%s.items[i], buffer, schema)) { return false; }\n", indent, element_struct, field->name);
            } else {
                char item_expr[256];
                snprintf(item_expr, sizeof(item_expr), "value->%s.items[i]", field->name);
                char inner_indent[32];
                snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
                append_scalar_encode(impl, schema, field, item_expr, inner_indent);
            }
            sb_append(impl, "%s}\n", indent);
        } else if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "%sif (!%s_encode_compact(&value->%s, buffer, schema)) { return false; }\n", indent, nested, field->name);
        } else {
            if (is_bitfield) {
                char ctype_buf[256];
                const char *ctype = c_type_for(schema, field->type_name, ctype_buf, sizeof(ctype_buf), field->line);
                char inner_indent[64];
                snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
                sb_append(impl, "%s{\n", indent);
                sb_append(impl, "%s%s bw_value;\n", indent, ctype);
                append_bit_width_assign(impl, schema, field, field_expr, "bw_value", inner_indent);
                sb_append(impl, "%s    if (!%sbitwriter_write(&bit_writer, buffer, (uint64_t)bw_value, %u)) { return false; }\n", indent, schema->prefix, field->bit_width);
                sb_append(impl, "%s}\n", indent);
            } else {
                append_scalar_encode(impl, schema, field, field_expr, indent);
            }
        }

        if (field->is_optional) {
            sb_append(impl, "    }\n");
        }
    }
    if (has_bitfields) {
        sb_append(impl, "    if (!%sbitwriter_flush(&bit_writer, buffer)) { return false; }\n", schema->prefix);
    }
    sb_append(impl, "    return true;\n}\n\n");

    sb_append(impl, "bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema) {\n", struct_name, struct_name, buffer_type, buffer_type, schema->prefix);
    sb_append(impl, "    if (!memory) { return false; }\n");
    sb_append(impl, "    memset(value, 0, sizeof(*value));\n");
    if (has_bitfields) {
        sb_append(impl, "    %sBitReader bit_reader = {0, 8};\n", schema->prefix);
    }
    
    // Slow path logic
    sb_append(impl,
        "    if (schema) {\n"
        "        const %sSchemaType *type = NULL;\n"
        "        for (uint32_t i = 0; i < schema->type_count; ++i) {\n"
        "            if (strcmp(schema->types[i].name, \"%s\") == 0) {\n"
        "                type = &schema->types[i];\n"
        "                break;\n"
        "            }\n"
        "        }\n"
        "        if (!type) return false;\n"
        "        uint32_t optional_count = 0;\n"
        "        for (uint32_t i = 0; i < type->field_count; ++i) {\n"
        "            if (type->fields[i].is_optional) optional_count++;\n"
        "        }\n"
        "        uint8_t mask_buf[128];\n"
        "        uint8_t *bitmask = NULL;\n"
        "        if (optional_count > 0) {\n"
        "            uint32_t bytes = (optional_count + 7) / 8;\n"
        "            if (bytes > sizeof(mask_buf)) return false;\n"
        "            if (!%sbuffer_read_bytes(buffer, mask_buf, bytes)) return false;\n"
        "            bitmask = mask_buf;\n"
        "        }\n"
        "        uint32_t opt_idx = 0;\n"
        "        for (uint32_t i = 0; i < type->field_count; ++i) {\n"
        "            bool present = true;\n"
        "            if (type->fields[i].is_optional) {\n"
        "                present = (bitmask[opt_idx / 8] >> (opt_idx %% 8)) & 1;\n"
        "                opt_idx++;\n"
        "            }\n"
        , schema->prefix, def->name, schema->prefix);
    if (has_bitfields) {
        sb_append(impl,
        "            if (type->fields[i].bit_width == 0) { %sbitreader_align(&bit_reader); }\n",
        schema->prefix);
    }
    sb_append(impl,
        "            if (!present) continue;\n"
        "            switch (type->fields[i].mapping) {\n");
        
    char enum_name[256];
    char tmp_name[256];
    strcpy(tmp_name, def->name);
    tmp_name[0] = (char)tolower((unsigned char)tmp_name[0]);
    snprintf(enum_name, sizeof(enum_name), "%s%sParameters", schema->prefix, tmp_name);

    for (size_t i = 0; i < def->field_count; ++i) {
        const FieldDef *field = &def->fields[i];
        sb_append(impl, "                case %s_%s: {\n", enum_name, field->name);
        if (field->is_optional) {
            sb_append(impl, "                    value->%sExist = true;\n", field->name);
        }
        
        const char *indent = "                    ";
        if (field->is_array) {
            char element_c_type[256];
            const char *element_c = c_type_for(schema, field->type_name, element_c_type, sizeof(element_c_type), field->line);
            char align_expr[256];
            snprintf(align_expr, sizeof(align_expr), "(sizeof(%s) > sizeof(void *)) ? sizeof(%s) : sizeof(void *)", element_c, element_c);
            sb_append(impl,
                "%s{\n"
                "%s    uint32_t count = 0;\n"
                "%s    if (!%sread_var_u32(buffer, &count)) { return false; }\n"
                "%s    value->%s.items = NULL;\n"
                "%s    value->%s.count = 0;\n"
                "%s    if (count) {\n"
                "%s        const uint32_t memory_marker = memory->used;\n"
                "%s        %s *items = (%s *)%sbuffer_push_aligned(memory, (size_t)count * sizeof(%s), %s);\n"
                "%s        if (!items) { return false; }\n",
                indent, indent, indent, schema->prefix, indent, field->name, indent, field->name, indent, indent, indent, element_c, element_c, schema->prefix, element_c, align_expr, indent);
            if (type_is_struct_type(schema, field->type_name)) {
                char element_struct[256];
                make_prefixed(element_struct, sizeof(element_struct), schema, field->type_name);
                sb_append(impl,
                    "%s        for (uint32_t k = 0; k < count; ++k) {\n"
                    "%s            if (!%s_decode_compact(&items[k], buffer, memory, schema)) {\n"
                    "%s                %sbuffer_pop_to(memory, memory_marker);\n"
                    "%s                return false;\n"
                    "%s            }\n"
                    "%s        }\n",
                    indent, indent, element_struct, indent, element_struct, indent, schema->prefix, indent, indent, indent);
            } else {
                char fail_action[256];
                snprintf(fail_action, sizeof(fail_action), "%sbuffer_pop_to(memory, memory_marker); return false;", schema->prefix);
                sb_append(impl,
                    "%s        for (uint32_t k = 0; k < count; ++k) {\n", indent);
                char inner_indent[32];
                snprintf(inner_indent, sizeof(inner_indent), "%s            ", indent);
                append_scalar_decode(impl, schema, field, "items[k]", inner_indent, fail_action);
                sb_append(impl,
                    "%s        }\n", indent);
            }
            sb_append(impl,
                "%s        value->%s.items = items;\n"
                "%s        value->%s.count = count;\n"
                "%s    }\n"
                "%s}\n",
                indent, field->name, indent, field->name, indent, indent);
        } else if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "%sif (!%s_decode_compact(&value->%s, buffer, memory, schema)) { return false; }\n", indent, nested, field->name);
        } else {
            char target[256];
            snprintf(target, sizeof(target), "value->%s", field->name);
            if (field->bit_width > 0) {
                char inner_indent[32];
                snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
                sb_append(impl,
                    "%s{\n"
                    "%s    uint64_t bw_raw = 0;\n"
                    "%s    if (!%sbitreader_read(&bit_reader, buffer, %u, &bw_raw)) { return false; }\n",
                    indent, indent, indent, schema->prefix, field->bit_width);
                append_bit_width_assign(impl, schema, field, "bw_raw", target, inner_indent);
                sb_append(impl, "%s}\n", indent);
            } else {
                append_scalar_decode(impl, schema, field, target, indent, NULL);
            }
        }
        sb_append(impl, "                    break;\n");
        sb_append(impl, "                }\n");
    }
    sb_append(impl,
        "                default:\n"
        "                    if (!%sskip_generic(buffer, type->fields[i].type_id, type->fields[i].is_array, schema)) return false;\n"
        "                    break;\n"
        "            }\n"
        "        }\n"
        "        return true;\n"
        "    }\n\n",
        schema->prefix);

    // Fast path logic (existing)
    if (optional_count > 0) {
        uint32_t bitmask_size = (optional_count + 7) / 8;
        sb_append(impl, "    uint8_t bitmask[%u];\n", bitmask_size);
        sb_append(impl, "    if (!%sbuffer_read_bytes(buffer, bitmask, sizeof(bitmask))) { return false; }\n", schema->prefix);
    }

    uint32_t opt_idx = 0;
    for (size_t i = 0; i < def->field_count; ++i) {
        const FieldDef *field = &def->fields[i];
        bool is_bitfield = field->bit_width > 0;
        
        if (field->is_optional) {
            sb_append(impl, "    value->%sExist = (bitmask[%u] >> %u) & 1u;\n", field->name, opt_idx / 8, opt_idx % 8);
            if (has_bitfields && !is_bitfield) {
                sb_append(impl, "    %sbitreader_align(&bit_reader);\n", schema->prefix);
            }
            sb_append(impl, "    if (value->%sExist) {\n", field->name);
            opt_idx++;
        } else {
            if (has_bitfields && !is_bitfield) {
                sb_append(impl, "    %sbitreader_align(&bit_reader);\n", schema->prefix);
            }
        }

        const char *indent = field->is_optional ? "        " : "    ";

        if (field->is_array) {
            char element_c_type[256];
            const char *element_c = c_type_for(schema, field->type_name, element_c_type, sizeof(element_c_type), field->line);
            char align_expr[256];
            snprintf(align_expr, sizeof(align_expr), "(sizeof(%s) > sizeof(void *)) ? sizeof(%s) : sizeof(void *)", element_c, element_c);
            sb_append(impl,
                "%s{\n"
                "%s    uint32_t count = 0;\n"
                "%s    if (!%sread_var_u32(buffer, &count)) { return false; }\n"
                "%s    value->%s.items = NULL;\n"
                "%s    value->%s.count = 0;\n"
                "%s    if (count) {\n"
                "%s        const uint32_t memory_marker = memory->used;\n"
                "%s        %s *items = (%s *)%sbuffer_push_aligned(memory, (size_t)count * sizeof(%s), %s);\n"
                "%s        if (!items) { return false; }\n",
                indent, indent, indent, schema->prefix, indent, field->name, indent, field->name, indent, indent, indent, element_c, element_c, schema->prefix, element_c, align_expr, indent);
            if (type_is_struct_type(schema, field->type_name)) {
                char element_struct[256];
                make_prefixed(element_struct, sizeof(element_struct), schema, field->type_name);
                sb_append(impl,
                    "%s        for (uint32_t i = 0; i < count; ++i) {\n"
                    "%s            if (!%s_decode_compact(&items[i], buffer, memory, NULL)) {\n"
                    "%s                %sbuffer_pop_to(memory, memory_marker);\n"
                    "%s                return false;\n"
                    "%s            }\n"
                    "%s        }\n",
                    indent, indent, element_struct, indent, element_struct, indent, schema->prefix, indent, indent, indent);
            } else {
                char fail_action[256];
                snprintf(fail_action, sizeof(fail_action), "%sbuffer_pop_to(memory, memory_marker); return false;", schema->prefix);
                sb_append(impl,
                    "%s        for (uint32_t i = 0; i < count; ++i) {\n", indent);
                char inner_indent[32];
                snprintf(inner_indent, sizeof(inner_indent), "%s            ", indent);
                append_scalar_decode(impl, schema, field, "items[i]", inner_indent, fail_action);
                sb_append(impl,
                    "%s        }\n", indent);
            }
            sb_append(impl,
                "%s        value->%s.items = items;\n"
                "%s        value->%s.count = count;\n"
                "%s    }\n"
                "%s}\n",
                indent, field->name, indent, field->name, indent, indent);
        } else if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "%sif (!%s_decode_compact(&value->%s, buffer, memory, NULL)) { return false; }\n", indent, nested, field->name);
        } else {
            char target[256];
            snprintf(target, sizeof(target), "value->%s", field->name);
            if (is_bitfield) {
                char inner_indent[32];
                snprintf(inner_indent, sizeof(inner_indent), "%s    ", indent);
                sb_append(impl,
                    "%s{\n"
                    "%s    uint64_t bw_raw = 0;\n"
                    "%s    if (!%sbitreader_read(&bit_reader, buffer, %u, &bw_raw)) { return false; }\n",
                    indent, indent, indent, schema->prefix, field->bit_width);
                append_bit_width_assign(impl, schema, field, "bw_raw", target, inner_indent);
                sb_append(impl, "%s}\n", indent);
            } else {
                append_scalar_decode(impl, schema, field, target, indent, NULL);
            }
        }

        if (field->is_optional) {
            sb_append(impl, "    }\n");
        }
    }
    sb_append(impl, "    return true;\n}\n\n");

    sb_append(impl, "bool %s_skip_compact(%s *buffer) {\n", struct_name, buffer_type);
    sb_append(impl,
        "    while (true) {\n"
        "        uint32_t field_id = 0;\n"
        "        %swireType wire = %swireType_Varint;\n"
        "        if (!%sread_wire_tag(buffer, &field_id, &wire)) { return false; }\n"
        "        if (field_id == 0) { break; }\n"
        "        if (!%sskip_wire_value(buffer, wire)) { return false; }\n"
        "    }\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix);
}
static void append_message_codec(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const StructDef *def, const char *api_macro) {
    char buffer_type[256];
    snprintf(buffer_type, sizeof(buffer_type), "%sBuffer", schema->prefix);
    char struct_name[256];
    make_prefixed(struct_name, sizeof(struct_name), schema, def->name);

    sb_append(decls, "%s bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema);\n", api_macro, struct_name, struct_name, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema);\n", api_macro, struct_name, struct_name, buffer_type, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_skip_compact(%s *buffer);\n\n", api_macro, struct_name, buffer_type);

    sb_append(impl, "bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema) {\n", struct_name, struct_name, buffer_type, schema->prefix);
    sb_append(impl, "    (void)schema;\n");
    
    char enum_name[256];
    char tmp_name[256];
    strcpy(tmp_name, def->name);
    tmp_name[0] = (char)tolower((unsigned char)tmp_name[0]);
    snprintf(enum_name, sizeof(enum_name), "%s%sParameters", schema->prefix, tmp_name);

    for (size_t i = 0; i < def->field_count; ++i) {
        const FieldDef *field = &def->fields[i];
        
        char has_expr[256];
        if (field->is_optional) {
            snprintf(has_expr, sizeof(has_expr), "value->%sExist", field->name);
        } else if (field->is_array) {
            snprintf(has_expr, sizeof(has_expr), "value->%s.count > 0", field->name);
        } else {
            snprintf(has_expr, sizeof(has_expr), "true");
        }

        char field_enum[256];
        snprintf(field_enum, sizeof(field_enum), "%s_%s", enum_name, field->name);
        
        char wire_literal[256];
        if (field->is_array || type_is_struct_type(schema, field->type_name)) {
            snprintf(wire_literal, sizeof(wire_literal), "%swireType_LengthDelimited", schema->prefix);
        } else {
             const char *base = field_base_type(schema, field);
             if (strcmp(base, "f32") == 0) snprintf(wire_literal, sizeof(wire_literal), "%swireType_Fixed32", schema->prefix);
             else if (strcmp(base, "f64") == 0) snprintf(wire_literal, sizeof(wire_literal), "%swireType_Fixed64", schema->prefix);
             else snprintf(wire_literal, sizeof(wire_literal), "%swireType_Varint", schema->prefix);
        }

        sb_append(impl,
            "    if (%s) {\n"
            "        if (!%swrite_wire_tag(buffer, %s, %s)) { return false; }\n",
            has_expr, schema->prefix, field_enum, wire_literal);
        
        if (field->is_array) {
            sb_append(impl,
                "        uint32_t block_marker = 0;\n"
                "        if (!%sbuffer_begin_block(buffer, &block_marker)) { return false; }\n"
                "        if (!%swrite_var_u32(buffer, value->%s.count)) { return false; }\n",
                schema->prefix, schema->prefix, field->name);
            
            sb_append(impl, "        for (uint32_t i = 0; i < value->%s.count; ++i) {\n", field->name);
            
            if (type_is_struct_type(schema, field->type_name)) {
                char element_struct[256];
                make_prefixed(element_struct, sizeof(element_struct), schema, field->type_name);
                sb_append(impl, "            if (!%s_encode_compact(&value->%s.items[i], buffer, schema)) { return false; }\n", element_struct, field->name);
            } else {
                char item_expr[256];
                snprintf(item_expr, sizeof(item_expr), "value->%s.items[i]", field->name);
                append_scalar_encode(impl, schema, field, item_expr, "            ");
            }
            sb_append(impl, "        }\n");
            sb_append(impl,
                "        if (!%sbuffer_end_block(buffer, block_marker)) { return false; }\n",
                schema->prefix);
        } else if (type_is_struct_type(schema, field->type_name)) {
            sb_append(impl,
                "        uint32_t block_marker = 0;\n"
                "        if (!%sbuffer_begin_block(buffer, &block_marker)) { return false; }\n",
                schema->prefix);
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "        if (!%s_encode_compact(&value->%s, buffer, schema)) { return false; }\n", nested, field->name);
            sb_append(impl,
                "        if (!%sbuffer_end_block(buffer, block_marker)) { return false; }\n",
                schema->prefix);
        } else {
            char value_expr[256];
            snprintf(value_expr, sizeof(value_expr), "value->%s", field->name);
            append_scalar_encode(impl, schema, field, value_expr, "        ");
        }
        sb_append(impl, "    }\n");
    }
    sb_append(impl, "    if (!%swrite_var_u32(buffer, 0)) { return false; }\n    return true;\n}\n\n", schema->prefix);

    sb_append(impl, "bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema) {\n", struct_name, struct_name, buffer_type, buffer_type, schema->prefix);
    sb_append(impl, "    if (!memory) { return false; }\n");
    sb_append(impl, "    memset(value, 0, sizeof(*value));\n");
    sb_append(impl,
        "    while (true) {\n"
        "        uint32_t field_id = 0;\n"
        "        %swireType wire = %swireType_Varint;\n"
        "        if (!%sread_wire_tag(buffer, &field_id, &wire)) { return false; }\n"
        "        if (field_id == 0) { break; }\n"
        "        switch (field_id) {\n",
        schema->prefix, schema->prefix, schema->prefix);

    for (size_t i = 0; i < def->field_count; ++i) {
        const FieldDef *field = &def->fields[i];
        char field_enum[256];
        snprintf(field_enum, sizeof(field_enum), "%s_%s", enum_name, field->name);
        
        sb_append(impl, "            case %s: {\n", field_enum);
        
        if (field->is_optional) {
            sb_append(impl, "                value->%sExist = true;\n", field->name);
        }
        
        if (field->is_array) {
            sb_append(impl, "                if (wire != %swireType_LengthDelimited) { return false; }\n", schema->prefix);
            sb_append(impl, "                uint32_t block_end = 0;\n");
            sb_append(impl, "                if (!%sbuffer_begin_read_block(buffer, &block_end)) { return false; }\n", schema->prefix);
            sb_append(impl, "                uint32_t count = 0;\n");
            sb_append(impl, "                if (!%sread_var_u32(buffer, &count)) { return false; }\n", schema->prefix);
            
            char element_c_type[256];
            const char *element_c = c_type_for(schema, field->type_name, element_c_type, sizeof(element_c_type), field->line);
            char align_expr[256];
            snprintf(align_expr, sizeof(align_expr), "(sizeof(%s) > sizeof(void *)) ? sizeof(%s) : sizeof(void *)", element_c, element_c);
            
            sb_append(impl,
                "                value->%s.items = NULL;\n"
                "                value->%s.count = 0;\n"
                "                if (count) {\n"
                "                    const uint32_t memory_marker = memory->used;\n"
                "                    %s *items = (%s *)%sbuffer_push_aligned(memory, (size_t)count * sizeof(%s), %s);\n"
                "                    if (!items) { return false; }\n",
                field->name, field->name, element_c, element_c, schema->prefix, element_c, align_expr);
                
            sb_append(impl, "                    for (uint32_t i = 0; i < count; ++i) {\n");
            
            if (type_is_struct_type(schema, field->type_name)) {
                char element_struct[256];
                make_prefixed(element_struct, sizeof(element_struct), schema, field->type_name);
                sb_append(impl, "                        if (!%s_decode_compact(&items[i], buffer, memory, schema)) {\n", element_struct);
                sb_append(impl, "                            %sbuffer_pop_to(memory, memory_marker);\n", schema->prefix);
                sb_append(impl, "                            return false;\n");
                sb_append(impl, "                        }\n");
            } else {
                char fail_action[256];
                snprintf(fail_action, sizeof(fail_action), "%sbuffer_pop_to(memory, memory_marker); return false;", schema->prefix);
                append_scalar_decode(impl, schema, field, "items[i]", "                        ", fail_action);
            }
            sb_append(impl, "                    }\n");
            sb_append(impl,
                "                    value->%s.items = items;\n"
                "                    value->%s.count = count;\n"
                "                }\n"
                "                if (!%sbuffer_end_read_block(buffer, block_end)) { return false; }\n",
                field->name, field->name, schema->prefix);
        } else if (type_is_struct_type(schema, field->type_name)) {
            sb_append(impl, "                if (wire != %swireType_LengthDelimited) { return false; }\n", schema->prefix);
            sb_append(impl, "                uint32_t block_end = 0;\n");
            sb_append(impl, "                if (!%sbuffer_begin_read_block(buffer, &block_end)) { return false; }\n", schema->prefix);
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "                if (!%s_decode_compact(&value->%s, buffer, memory, schema)) { return false; }\n", nested, field->name);
            sb_append(impl, "                if (!%sbuffer_end_read_block(buffer, block_end)) { return false; }\n", schema->prefix);
        } else {
            char target[256];
            snprintf(target, sizeof(target), "value->%s", field->name);
            append_scalar_decode(impl, schema, field, target, "                ", "return false;");
        }
        sb_append(impl, "                break;\n            }\n");
    }
    
    sb_append(impl,
        "            default:\n"
        "                if (!%sskip_wire_value(buffer, wire)) { return false; }\n"
        "                break;\n"
        "        }\n"
        "    }\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix);

    sb_append(impl, "bool %s_skip_compact(%s *buffer) {\n", struct_name, buffer_type);
    sb_append(impl,
        "    while (true) {\n"
        "        uint32_t field_id = 0;\n"
        "        %swireType wire = %swireType_Varint;\n"
        "        if (!%sread_wire_tag(buffer, &field_id, &wire)) { return false; }\n"
        "        if (field_id == 0) { break; }\n"
        "        if (!%sskip_wire_value(buffer, wire)) { return false; }\n"
        "    }\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix);
}

static void append_union_codec(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const UnionDef *def, const char *api_macro) {
    char buffer_type[256];
    snprintf(buffer_type, sizeof(buffer_type), "%sBuffer", schema->prefix);
    char union_name[256];
    make_prefixed(union_name, sizeof(union_name), schema, def->name);
    char tag_enum[256];
    make_union_tag_enum_name(tag_enum, sizeof(tag_enum), schema, def);

    sb_append(decls, "%s bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema);\n", api_macro, union_name, union_name, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema);\n", api_macro, union_name, union_name, buffer_type, buffer_type, schema->prefix);
    sb_append(decls, "%s bool %s_skip_compact(%s *buffer);\n\n", api_macro, union_name, buffer_type);

    sb_append(impl, "bool %s_encode_compact(const %s *value, %s *buffer, const %sSchemaInfo *schema) {\n", union_name, union_name, buffer_type, schema->prefix);
    sb_append(impl, "    (void)schema;\n");
    sb_append(impl, "    if (!%swrite_var_u32(buffer, (uint32_t)value->tag)) { return false; }\n", schema->prefix);
    sb_append(impl, "    switch (value->tag) {\n");
    for (size_t i = 0; i < def->field_count; ++i) {
        const UnionField *field = &def->fields[i];
        sb_append(impl, "        case %s_%s: {\n", tag_enum, field->name);
        if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "            return %s_encode_compact(&value->value.%s, buffer, schema);\n", nested, field->name);
        } else {
            FieldDef tmp = {0};
            tmp.type_name = field->type_name;
            tmp.line = field->line;
            tmp.bit_width = 0;
            char value_expr[256];
            snprintf(value_expr, sizeof(value_expr), "value->value.%s", field->name);
            append_scalar_encode(impl, schema, &tmp, value_expr, "            ");
            sb_append(impl, "            return true;\n");
        }
        sb_append(impl, "        }\n");
    }
    sb_append(impl, "        default: return false;\n    }\n}\n\n");

    sb_append(impl, "bool %s_decode_compact(%s *value, %s *buffer, %s *memory, const %sSchemaInfo *schema) {\n", union_name, union_name, buffer_type, buffer_type, schema->prefix);
    sb_append(impl, "    (void)schema;\n    if (!memory) { return false; }\n    memset(value, 0, sizeof(*value));\n");
    sb_append(impl, "    uint32_t tag = 0;\n    if (!%sread_var_u32(buffer, &tag)) { return false; }\n", schema->prefix);
    sb_append(impl, "    value->tag = (%s)tag;\n", tag_enum);
    sb_append(impl, "    switch (value->tag) {\n");
    for (size_t i = 0; i < def->field_count; ++i) {
        const UnionField *field = &def->fields[i];
        sb_append(impl, "        case %s_%s: {\n", tag_enum, field->name);
        if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "            return %s_decode_compact(&value->value.%s, buffer, memory, schema);\n", nested, field->name);
        } else {
            FieldDef tmp = {0};
            tmp.type_name = field->type_name;
            tmp.line = field->line;
            tmp.bit_width = 0;
            char target[256];
            snprintf(target, sizeof(target), "value->value.%s", field->name);
            append_scalar_decode(impl, schema, &tmp, target, "            ", "return false;");
            sb_append(impl, "            return true;\n");
        }
        sb_append(impl, "        }\n");
    }
    sb_append(impl, "        default: return false;\n    }\n}\n\n");

    sb_append(impl, "bool %s_skip_compact(%s *buffer) {\n", union_name, buffer_type);
    sb_append(impl, "    uint32_t tag = 0;\n    if (!%sread_var_u32(buffer, &tag)) { return false; }\n", schema->prefix);
    sb_append(impl, "    switch (tag) {\n");
    for (size_t i = 0; i < def->field_count; ++i) {
        const UnionField *field = &def->fields[i];
        sb_append(impl, "        case %s_%s: {\n", tag_enum, field->name);
        FieldDef tmp = {0};
        tmp.type_name = field->type_name;
        tmp.line = field->line;
        const char *base = field_base_type(schema, &tmp);
        if (type_is_struct_type(schema, field->type_name)) {
            char nested[256];
            make_prefixed(nested, sizeof(nested), schema, field->type_name);
            sb_append(impl, "            return %s_skip_compact(buffer);\n", nested);
        } else if (type_is_string(field->type_name)) {
            sb_append(impl, "            uint32_t len = 0;\n            if (!%sread_var_u32(buffer, &len)) { return false; }\n            return %sbuffer_skip_bytes(buffer, len);\n", schema->prefix, schema->prefix);
        } else if (type_is_f32(base)) {
            sb_append(impl, "            float tmp = 0.0f; return %sread_compact_f32(buffer, &tmp);\n", schema->prefix);
        } else if (type_is_f64(base)) {
            sb_append(impl, "            double tmp = 0.0; return %sread_compact_f64(buffer, &tmp);\n", schema->prefix);
        } else if (type_is_u64(base)) {
            sb_append(impl, "            uint64_t tmp = 0; return %sread_var_u64(buffer, &tmp);\n", schema->prefix);
        } else if (type_is_u32_compatible(base) || type_is_bool(base) || type_is_u8(base)) {
            sb_append(impl, "            uint32_t tmp = 0; return %sread_var_u32(buffer, &tmp);\n", schema->prefix);
        } else if (type_is_signed64(base)) {
            sb_append(impl, "            int64_t tmp = 0; return %sread_var_s64(buffer, &tmp);\n", schema->prefix);
        } else {
            sb_append(impl, "            int32_t tmp = 0; return %sread_var_s32(buffer, &tmp);\n", schema->prefix);
        }
        sb_append(impl, "        }\n");
    }
    sb_append(impl, "        default: return false;\n    }\n}\n\n");
}
static void append_compact_codecs(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        if (def->kind == STRUCT_KIND_STRUCT) {
            append_struct_codec(decls, impl, schema, def, api_macro);
        }
    }
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        if (def->kind == STRUCT_KIND_MESSAGE) {
            append_message_codec(decls, impl, schema, def, api_macro);
        }
    }
    for (size_t i = 0; i < schema->union_count; ++i) {
        append_union_codec(decls, impl, schema, &schema->unions[i], api_macro);
    }
}

// MARK: Buffer and wire helpers

static void append_memory_buffer(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    sb_append(decls,
        "typedef struct %sBuffer {\n"
        "    void *ptr;\n"
        "    uint32_t size;\n"
        "    uint32_t used;\n"
        "} %sBuffer;\n\n"
        "typedef struct %sSchemaInfo %sSchemaInfo;\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix);

    if (!schema->string_alias_overridden) {
        char str_type[128];
        make_string_typename(str_type, sizeof(str_type), schema);
        sb_append(decls,
            "typedef struct %s {\n"
            "    uint8_t *content;\n"
            "    uint64_t size;\n"
            "} %s;\n\n",
            str_type, str_type);
    }
    
    sb_append(decls, "%s void %sbuffer_init(%sBuffer *buffer, void *ptr, uint32_t size);\n", api_macro, schema->prefix, schema->prefix);
    sb_append(impl,
        "void %sbuffer_init(%sBuffer *buffer, void *ptr, uint32_t size) {\n"
        "    buffer->ptr = ptr;\n"
        "    buffer->size = size;\n"
        "    buffer->used = 0;\n"
        "}\n\n",
        schema->prefix, schema->prefix);

    sb_append(impl, "bool %sbuffer_read_bytes(%sBuffer *buffer, void *dst, size_t len) {\n", schema->prefix, schema->prefix);
    sb_append(impl,
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    const uint8_t *base = (const uint8_t *)buffer->ptr;\n"
        "    memcpy(dst, base + buffer->used, len);\n"
        "    buffer->used += (uint32_t)len;\n"
        "    return true;\n"
        "}\n\n");

    sb_append(impl, "bool %sbuffer_write_bytes(%sBuffer *buffer, const void *src, size_t len) {\n", schema->prefix, schema->prefix);
    sb_append(impl,
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    uint8_t *base = (uint8_t *)buffer->ptr;\n"
        "    memcpy(base + buffer->used, src, len);\n"
        "    buffer->used += (uint32_t)len;\n"
        "    return true;\n"
        "}\n\n");

    sb_append(impl, "size_t %sbuffer_align_forward(size_t value, size_t alignment) {\n", schema->prefix);
    sb_append(impl,
        "    if (alignment < sizeof(void *)) { alignment = sizeof(void *); }\n"
        "    size_t remainder = value %% alignment;\n"
        "    if (remainder) { value += alignment - remainder; }\n"
        "    return value;\n"
        "}\n\n");

    sb_append(impl, "void *%sbuffer_push_aligned(%sBuffer *buffer, size_t len, size_t alignment) {\n", schema->prefix, schema->prefix);
    sb_append(impl,
        "    if (!buffer) { return NULL; }\n"
        "    size_t offset = %sbuffer_align_forward((size_t)buffer->used, alignment);\n"
        "    size_t end = offset + len;\n"
        "    if (end > (size_t)buffer->size) { return NULL; }\n"
        "    uint8_t *base = (uint8_t *)buffer->ptr;\n"
        "    void *ptr = base + offset;\n"
        "    buffer->used = (uint32_t)end;\n"
        "    return ptr;\n"
        "}\n\n",
        schema->prefix);

    sb_append(impl, "bool %sbuffer_pop_to(%sBuffer *buffer, uint32_t marker) {\n", schema->prefix, schema->prefix);
    sb_append(impl,
        "    if (!buffer) { return false; }\n"
        "    if (marker > buffer->used) { return false; }\n"
        "    buffer->used = marker;\n"
        "    return true;\n"
        "}\n\n");
}

static void append_buffer_helpers(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    // Pointer aliases (e.g., string/char*) need custom handling; skip them here.
    // 'string' is emitted via append_string_helpers below.
    for (size_t i = 0; i < schema->alias_count; ++i) {
        const TypeAlias *alias = &schema->aliases[i];
        if (strcmp(alias->name, "string") == 0) { continue; }
        if (strchr(alias->c_type, '*')) { continue; }
        const char *swap_func = NULL;
        if (strcmp(alias->c_type, "uint16_t") == 0 || strcmp(alias->c_type, "int16_t") == 0) {
            swap_func = "bswap16";
        } else if (strcmp(alias->c_type, "uint32_t") == 0 || strcmp(alias->c_type, "int32_t") == 0) {
            swap_func = "bswap32";
        } else if (strcmp(alias->c_type, "uint64_t") == 0 || strcmp(alias->c_type, "int64_t") == 0) {
            swap_func = "bswap64";
        }

        if (swap_func) {
            const char *unsigned_type = "uint16_t";
            if (strstr(alias->c_type, "32")) unsigned_type = "uint32_t";
            if (strstr(alias->c_type, "64")) unsigned_type = "uint64_t";

            sb_append(impl,
                "bool %sbuffer_read_%s(%sBuffer *buffer, %s *out) {\n"
                "    if (!%sbuffer_read_bytes(buffer, out, sizeof(*out))) { return false; }\n"
                "#if %s_IS_BIG_ENDIAN\n"
                "    *out = (%s)%s_%s((%s)*out);\n"
                "#endif\n"
                "    return true;\n"
                "}\n",
                schema->prefix, alias->name, schema->prefix, alias->c_type,
                schema->prefix, schema->prefix, alias->c_type, schema->prefix, swap_func, unsigned_type);

            sb_append(impl,
                "bool %sbuffer_write_%s(%sBuffer *buffer, const %s value) {\n"
                "#if %s_IS_BIG_ENDIAN\n"
                "    %s swapped = (%s)%s_%s((%s)value);\n"
                "    return %sbuffer_write_bytes(buffer, &swapped, sizeof(swapped));\n"
                "#else\n"
                "    return %sbuffer_write_bytes(buffer, &value, sizeof(value));\n"
                "#endif\n"
                "}\n\n",
                schema->prefix, alias->name, schema->prefix, alias->c_type,
                schema->prefix, alias->c_type, alias->c_type, schema->prefix, swap_func, unsigned_type,
                schema->prefix, schema->prefix);
        } else {
             sb_append(impl,
                "bool %sbuffer_read_%s(%sBuffer *buffer, %s *out) {\n"
                "    return %sbuffer_read_bytes(buffer, out, sizeof(*out));\n"
                "}\n",
                schema->prefix, alias->name, schema->prefix, alias->c_type, schema->prefix);

            sb_append(impl,
                "bool %sbuffer_write_%s(%sBuffer *buffer, const %s value) {\n"
                "    return %sbuffer_write_bytes(buffer, &value, sizeof(value));\n"
                "}\n\n",
                schema->prefix, alias->name, schema->prefix, alias->c_type, schema->prefix);
        }
    }
}

static void append_string_helpers(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    const TypeAlias *alias = find_alias_const(schema, "string");
    if (!alias) { return; }
    char str_type[256];
    snprintf(str_type, sizeof(str_type), "%s", alias->c_type);

    sb_append(impl,
        "bool %sbuffer_read_string(%sBuffer *buffer, %sBuffer *memory, %s *out) {\n"
        "    if (!memory) { return false; }\n"
        "    uint32_t len = 0;\n"
        "    if (!%sread_var_u32(buffer, &len)) { return false; }\n"
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    uint8_t *str = (uint8_t *)%sbuffer_push_aligned(memory, (size_t)len + 1, 1);\n"
        "    if (!str) { return false; }\n"
        "    if (!%sbuffer_read_bytes(buffer, str, len)) { return false; }\n"
        "    str[len] = 0;\n"
        "    out->content = str;\n"
        "    out->size = len;\n"
        "    return true;\n"
        "}\n",
        schema->prefix, schema->prefix, schema->prefix, str_type, schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sbuffer_write_string(%sBuffer *buffer, const %s *value) {\n"
        "    uint64_t len = value ? value->size : 0;\n"
        "    if (len > UINT32_MAX) { return false; }\n"
        "    if (!%swrite_var_u32(buffer, (uint32_t)len)) { return false; }\n"
        "    if (len == 0) { return true; }\n"
        "    if (!value->content) { return false; }\n"
        "    return %sbuffer_write_bytes(buffer, value->content, (size_t)len);\n"
        "}\n\n",
        schema->prefix, schema->prefix, str_type, schema->prefix, schema->prefix);
}

static void append_wire_helpers(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    sb_append(decls,
        "typedef enum %swireType {\n"
        "    %swireType_Varint = 0,\n"
        "    %swireType_Fixed32 = 1,\n"
        "    %swireType_Fixed64 = 2,\n"
        "    %swireType_LengthDelimited = 3,\n"
        "} %swireType;\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sbuffer_skip_bytes(%sBuffer *buffer, size_t len) {\n"
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    buffer->used += (uint32_t)len;\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix);

    sb_append(impl,
        "uint32_t %sencode_var_u32_raw(uint32_t value, uint8_t out[5]) {\n"
        "    uint32_t count = 0;\n"
        "    do {\n"
        "        uint8_t byte = (uint8_t)(value & 0x7Fu);\n"
        "        value >>= 7u;\n"
        "        if (value) { byte |= 0x80u; }\n"
        "        out[count++] = byte;\n"
        "    } while (value);\n"
        "    return count;\n"
        "}\n\n",
        schema->prefix);

    sb_append(impl,
        "bool %sbuffer_begin_block(%sBuffer *buffer, uint32_t *marker) {\n"
        "    if (buffer->used + 5 > buffer->size) { return false; }\n"
        "    uint8_t *base = (uint8_t *)buffer->ptr;\n"
        "    memset(base + buffer->used, 0, 5);\n"
        "    *marker = buffer->used;\n"
        "    buffer->used += 5;\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sbuffer_end_block(%sBuffer *buffer, uint32_t marker) {\n"
        "    uint32_t payload_start = marker + 5;\n"
        "    if (buffer->used < payload_start) { return false; }\n"
        "    uint32_t payload_len = buffer->used - payload_start;\n"
        "    uint8_t encoded[5];\n"
        "    uint32_t encoded_len = %sencode_var_u32_raw(payload_len, encoded);\n"
        "    uint8_t *base = (uint8_t *)buffer->ptr;\n"
        "    memmove(base + marker + encoded_len, base + payload_start, payload_len);\n"
        "    memcpy(base + marker, encoded, encoded_len);\n"
        "    buffer->used = marker + encoded_len + payload_len;\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl, "bool %swrite_var_u32(%sBuffer *buffer, uint32_t value);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %sread_var_u32(%sBuffer *buffer, uint32_t *out);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %swrite_var_u64(%sBuffer *buffer, uint64_t value);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %sread_var_u64(%sBuffer *buffer, uint64_t *out);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %swrite_var_s32(%sBuffer *buffer, int32_t value);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %sread_var_s32(%sBuffer *buffer, int32_t *out);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %swrite_var_s64(%sBuffer *buffer, int64_t value);\n", schema->prefix, schema->prefix);
    sb_append(impl, "bool %sread_var_s64(%sBuffer *buffer, int64_t *out);\n\n", schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sbuffer_begin_read_block(%sBuffer *buffer, uint32_t *block_end) {\n"
        "    if (!block_end) { return false; }\n"
        "    uint32_t len = 0;\n"
        "    if (!%sread_var_u32(buffer, &len)) { return false; }\n"
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    *block_end = buffer->used + len;\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sbuffer_end_read_block(%sBuffer *buffer, uint32_t block_end) {\n"
        "    if (block_end > buffer->size) { return false; }\n"
        "    if (buffer->used > block_end) { return false; }\n"
        "    if (buffer->used < block_end) {\n"
        "        return %sbuffer_skip_bytes(buffer, (size_t)(block_end - buffer->used));\n"
        "    }\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "uint32_t %smake_wire_tag(uint32_t field_id, %swireType wire) {\n"
        "    return (field_id << 2) | (uint32_t)wire;\n"
        "}\n",
        schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %swrite_wire_tag(%sBuffer *buffer, uint32_t field_id, %swireType wire) {\n"
        "    return %swrite_var_u32(buffer, %smake_wire_tag(field_id, wire));\n"
        "}\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sread_wire_tag(%sBuffer *buffer, uint32_t *out_field_id, %swireType *out_wire) {\n"
        "    uint32_t raw = 0;\n"
        "    if (!%sread_var_u32(buffer, &raw)) { return false; }\n"
        "    if (raw == 0) { *out_field_id = 0; *out_wire = %swireType_Varint; return true; }\n"
        "    *out_field_id = raw >> 2;\n"
        "    *out_wire = (%swireType)(raw & 0x3u);\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "bool %sskip_wire_value(%sBuffer *buffer, %swireType wire) {\n"
        "    switch (wire) {\n"
        "        case %swireType_Varint:\n"
        "        {\n"
        "            while (true) {\n"
        "                uint8_t byte = 0;\n"
        "                if (!%sbuffer_read_u8(buffer, &byte)) { return false; }\n"
        "                if ((byte & 0x80u) == 0) { break; }\n"
        "            }\n"
        "            return true;\n"
        "        }\n"
        "        case %swireType_Fixed32:\n"
        "            return %sbuffer_skip_bytes(buffer, 4);\n"
        "        case %swireType_Fixed64:\n"
        "            return %sbuffer_skip_bytes(buffer, 8);\n"
        "        case %swireType_LengthDelimited:\n"
        "        {\n"
        "            uint32_t len = 0;\n"
        "            if (!%sread_var_u32(buffer, &len)) { return false; }\n"
        "            return %sbuffer_skip_bytes(buffer, len);\n"
        "        }\n"
        "        default:\n"
        "            return false;\n"
        "    }\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix,
        schema->prefix);
}

static void append_compact_helpers(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    const char *p = schema->prefix;
    
    sb_append(impl,
        "typedef struct %sBitWriter {\n"
        "    uint8_t scratch;\n"
        "    uint8_t used_bits;\n"
        "} %sBitWriter;\n"
        "typedef struct %sBitReader {\n"
        "    uint8_t scratch;\n"
        "    uint8_t used_bits;\n"
        "} %sBitReader;\n"
        "bool %sbitwriter_write(%sBitWriter *bw, %sBuffer *buffer, uint64_t value, uint32_t width) {\n"
        "    while (width) {\n"
        "        uint32_t avail = 8u - bw->used_bits;\n"
        "        uint32_t take = width < avail ? width : avail;\n"
        "        uint8_t chunk_mask = (uint8_t)((UINT8_C(1) << take) - 1u);\n"
        "        bw->scratch |= (uint8_t)((value & chunk_mask) << bw->used_bits);\n"
        "        bw->used_bits += (uint8_t)take;\n"
        "        value >>= take;\n"
        "        width -= take;\n"
        "        if (bw->used_bits == 8u) {\n"
        "            if (!%sbuffer_write_u8(buffer, bw->scratch)) { return false; }\n"
        "            bw->scratch = 0;\n"
        "            bw->used_bits = 0;\n"
        "        }\n"
        "    }\n"
        "    return true;\n"
        "}\n"
        "bool %sbitwriter_flush(%sBitWriter *bw, %sBuffer *buffer) {\n"
        "    if (bw->used_bits == 0) { return true; }\n"
        "    bool ok = %sbuffer_write_u8(buffer, bw->scratch);\n"
        "    bw->scratch = 0;\n"
        "    bw->used_bits = 0;\n"
        "    return ok;\n"
        "}\n"
        "bool %sbitreader_read(%sBitReader *br, %sBuffer *buffer, uint32_t width, uint64_t *out) {\n"
        "    uint64_t result = 0;\n"
        "    uint32_t shift = 0;\n"
        "    while (width) {\n"
        "        if (br->used_bits == 8u) {\n"
        "            if (!%sbuffer_read_u8(buffer, &br->scratch)) { return false; }\n"
        "            br->used_bits = 0;\n"
        "        }\n"
        "        uint32_t avail = 8u - br->used_bits;\n"
        "        uint32_t take = width < avail ? width : avail;\n"
        "        uint8_t chunk_mask = (uint8_t)((UINT8_C(1) << take) - 1u);\n"
        "        uint8_t chunk = (uint8_t)((br->scratch >> br->used_bits) & chunk_mask);\n"
        "        result |= ((uint64_t)chunk) << shift;\n"
        "        br->used_bits += (uint8_t)take;\n"
        "        width -= take;\n"
        "        shift += take;\n"
        "    }\n"
        "    *out = result;\n"
        "    return true;\n"
        "}\n"
        "void %sbitreader_align(%sBitReader *br) {\n"
        "    if (br->used_bits != 0 && br->used_bits < 8u) { br->used_bits = 8u; }\n"
        "}\n\n",
        p, p, p, p,
        p, p, p,
        p, p,
        p, p, p, p,
        p, p, p, p,
        p, p);

    sb_append(impl,
        "uint32_t %szigzag32(int32_t value) {\n"
        "    return ((uint32_t)value << 1) ^ (uint32_t)(value >> 31);\n"
        "}\n"
        "int32_t %sunzigzag32(uint32_t value) {\n"
        "    return (int32_t)((value >> 1) ^ (~(value & 1) + 1));\n"
        "}\n"
        "uint64_t %szigzag64(int64_t value) {\n"
        "    return ((uint64_t)value << 1) ^ (uint64_t)(value >> 63);\n"
        "}\n"
        "int64_t %sunzigzag64(uint64_t value) {\n"
        "    return (int64_t)((value >> 1) ^ (~(value & 1) + 1));\n"
        "}\n\n",
        p, p, p, p);

    sb_append(impl,
        "bool %swrite_var_u32(%sBuffer *buffer, uint32_t value) {\n"
        "    uint8_t bytes[5];\n"
        "    uint32_t count = 0;\n"
        "    do {\n"
        "        uint8_t byte = (uint8_t)(value & 0x7Fu);\n"
        "        value >>= 7u;\n"
        "        if (value) { byte |= 0x80u; }\n"
        "        bytes[count++] = byte;\n"
        "    } while (value && count < sizeof(bytes));\n"
        "    return %sbuffer_write_bytes(buffer, bytes, count);\n"
        "}\n",
        p, p, p);

    sb_append(impl,
        "bool %sread_var_u32(%sBuffer *buffer, uint32_t *out) {\n"
        "    uint32_t result = 0;\n"
        "    uint32_t shift = 0;\n"
        "    for (uint32_t i = 0; i < 5; ++i) {\n"
        "        uint8_t byte = 0;\n"
        "        if (!%sbuffer_read_u8(buffer, &byte)) { return false; }\n"
        "        result |= ((uint32_t)(byte & 0x7Fu) << shift);\n"
        "        if ((byte & 0x80u) == 0) { *out = result; return true; }\n"
        "        shift += 7u;\n"
        "    }\n"
        "    return false;\n"
        "}\n",
        p, p, p);

    sb_append(impl,
        "bool %swrite_var_u64(%sBuffer *buffer, uint64_t value) {\n"
        "    uint8_t bytes[10];\n"
        "    uint32_t count = 0;\n"
        "    do {\n"
        "        uint8_t byte = (uint8_t)(value & 0x7Fu);\n"
        "        value >>= 7u;\n"
        "        if (value) { byte |= 0x80u; }\n"
        "        bytes[count++] = byte;\n"
        "    } while (value && count < sizeof(bytes));\n"
        "    return %sbuffer_write_bytes(buffer, bytes, count);\n"
        "}\n",
        p, p, p);

    sb_append(impl,
        "bool %sread_var_u64(%sBuffer *buffer, uint64_t *out) {\n"
        "    uint64_t result = 0;\n"
        "    uint32_t shift = 0;\n"
        "    for (uint32_t i = 0; i < 10; ++i) {\n"
        "        uint8_t byte = 0;\n"
        "        if (!%sbuffer_read_u8(buffer, &byte)) { return false; }\n"
        "        result |= ((uint64_t)(byte & 0x7Fu) << shift);\n"
        "        if ((byte & 0x80u) == 0) { *out = result; return true; }\n"
        "        shift += 7u;\n"
        "    }\n"
        "    return false;\n"
        "}\n",
        p, p, p);

    sb_append(impl,
        "bool %swrite_var_s32(%sBuffer *buffer, int32_t value) {\n"
        "    return %swrite_var_u32(buffer, %szigzag32(value));\n"
        "}\n"
        "bool %sread_var_s32(%sBuffer *buffer, int32_t *out) {\n"
        "    uint32_t tmp = 0;\n"
        "    if (!%sread_var_u32(buffer, &tmp)) { return false; }\n"
        "    *out = %sunzigzag32(tmp);\n"
        "    return true;\n"
        "}\n",
        p, p, p, p, p, p, p, p);

    sb_append(impl,
        "bool %swrite_var_s64(%sBuffer *buffer, int64_t value) {\n"
        "    return %swrite_var_u64(buffer, %szigzag64(value));\n"
        "}\n"
        "bool %sread_var_s64(%sBuffer *buffer, int64_t *out) {\n"
        "    uint64_t tmp = 0;\n"
        "    if (!%sread_var_u64(buffer, &tmp)) { return false; }\n"
        "    *out = %sunzigzag64(tmp);\n"
        "    return true;\n"
        "}\n",
        p, p, p, p, p, p, p, p);

    sb_append(impl,
        "bool %sbuffer_skip(%sBuffer *buffer, size_t len) {\n"
        "    if (buffer->used + len > buffer->size) { return false; }\n"
        "    buffer->used += (uint32_t)len;\n"
        "    return true;\n"
        "}\n",
        p, p);

    sb_append(impl,
        "bool %swrite_field_header(%sBuffer *buffer, uint32_t field_id, uint32_t type_code) {\n"
        "    if (!%swrite_var_u32(buffer, field_id)) { return false; }\n"
        "    if (field_id == 0) { return true; }\n"
        "    return %swrite_var_u32(buffer, type_code);\n"
        "}\n"
        "bool %sread_field_header(%sBuffer *buffer, uint32_t *out_field, uint32_t *out_type) {\n"
        "    if (!%sread_var_u32(buffer, out_field)) { return false; }\n"
        "    if (*out_field == 0) { return true; }\n"
        "    return %sread_var_u32(buffer, out_type);\n"
        "}\n",
        p, p, p, p, p, p, p, p);

    sb_append(impl,
        "float %sdenorm_clamp_f32(float value) {\n"
        "    if (fabsf(value) < FLT_MIN) { return 0.0f; }\n"
        "    return value;\n"
        "}\n"
        "bool %swrite_compact_f32(%sBuffer *buffer, float value) {\n"
        "    float normalized = %sdenorm_clamp_f32(value);\n"
        "    uint8_t flag = normalized == 0.0f ? 0u : 1u;\n"
        "    if (!%sbuffer_write_u8(buffer, flag)) { return false; }\n"
        "    if (!flag) { return true; }\n"
        "#if %s_IS_BIG_ENDIAN\n"
        "    uint32_t raw; memcpy(&raw, &normalized, 4); raw = %s_bswap32(raw);\n"
        "    return %sbuffer_write_bytes(buffer, &raw, sizeof(raw));\n"
        "#else\n"
        "    return %sbuffer_write_bytes(buffer, &normalized, sizeof(normalized));\n"
        "#endif\n"
        "}\n"
        "bool %sread_compact_f32(%sBuffer *buffer, float *out) {\n"
        "    uint8_t flag = 0;\n"
        "    if (!%sbuffer_read_u8(buffer, &flag)) { return false; }\n"
        "    if (!flag) { *out = 0.0f; return true; }\n"
        "#if %s_IS_BIG_ENDIAN\n"
        "    uint32_t raw; if (!%sbuffer_read_bytes(buffer, &raw, sizeof(raw))) return false;\n"
        "    raw = %s_bswap32(raw); memcpy(out, &raw, 4); return true;\n"
        "#else\n"
        "    return %sbuffer_read_bytes(buffer, out, sizeof(*out));\n"
        "#endif\n"
        "}\n",
        p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p);

    sb_append(impl,
        "bool %swrite_compact_f64(%sBuffer *buffer, double value) {\n"
        "    double normalized = fabs(value) < DBL_MIN ? 0.0 : value;\n"
        "    uint8_t flag = normalized == 0.0 ? 0u : 1u;\n"
        "    if (!%sbuffer_write_u8(buffer, flag)) { return false; }\n"
        "    if (!flag) { return true; }\n"
        "#if %s_IS_BIG_ENDIAN\n"
        "    uint64_t raw; memcpy(&raw, &normalized, 8); raw = %s_bswap64(raw);\n"
        "    return %sbuffer_write_bytes(buffer, &raw, sizeof(raw));\n"
        "#else\n"
        "    return %sbuffer_write_bytes(buffer, &normalized, sizeof(normalized));\n"
        "#endif\n"
        "}\n"
        "bool %sread_compact_f64(%sBuffer *buffer, double *out) {\n"
        "    uint8_t flag = 0;\n"
        "    if (!%sbuffer_read_u8(buffer, &flag)) { return false; }\n"
        "    if (!flag) { *out = 0.0; return true; }\n"
        "#if %s_IS_BIG_ENDIAN\n"
        "    uint64_t raw; if (!%sbuffer_read_bytes(buffer, &raw, sizeof(raw))) return false;\n"
        "    raw = %s_bswap64(raw); memcpy(out, &raw, 8); return true;\n"
        "#else\n"
        "    return %sbuffer_read_bytes(buffer, out, sizeof(*out));\n"
        "#endif\n"
        "}\n",
        p, p, p, p, p, p, p, p, p, p, p, p, p, p, p);
}

// MARK: Type definitions

static void append_enum_defs(StringBuilder *out, const Schema *schema) {
    for (size_t i = 0; i < schema->enum_count; ++i) {
        const EnumDef *def = &schema->enums[i];
        char enum_name[256];
        make_prefixed(enum_name, sizeof(enum_name), schema, def->name);
        char base_buf[256];
        const char *base_ctype = def->is_bitset ? c_type_for(schema, def->base_type, base_buf, sizeof(base_buf), def->line) : NULL;
        if (def->is_bitset) {
            sb_append(out, "typedef %s %s;\n", base_ctype, enum_name);
            sb_append(out, "enum {\n");
        } else {
            sb_append(out, "typedef enum %s {\n", enum_name);
        }
        for (size_t j = 0; j < def->value_count; ++j) {
            char value_name[256];
            make_enum_value_name(value_name, sizeof(value_name), schema, def->name, def->values[j].name);
            sb_append(out, "    %s = %llu,\n", value_name, (unsigned long long)def->values[j].value);
        }
        if (def->is_bitset) {
            sb_append(out, "};\n\n");
        } else {
            sb_append(out, "} %s;\n\n", enum_name);
        }
    }
}

static void append_array_forward(StringBuilder *out, const Schema *schema) {
    for (size_t i = 0; i < schema->array_count; ++i) {
        char array_name[256];
        make_array_name(array_name, sizeof(array_name), schema, schema->arrays[i].element_type);
        sb_append(out, "typedef struct %s %s;\n", array_name, array_name);
    }
    if (schema->array_count) { sb_append(out, "\n"); }
}

static void append_struct_forward(StringBuilder *out, const Schema *schema) {
    for (size_t i = 0; i < schema->struct_count; ++i) {
        char struct_name[256];
        make_prefixed(struct_name, sizeof(struct_name), schema, schema->structs[i].name);
        sb_append(out, "typedef struct %s %s;\n", struct_name, struct_name);
    }
    for (size_t i = 0; i < schema->union_count; ++i) {
        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, schema->unions[i].name);
        sb_append(out, "typedef struct %s %s;\n", union_name, union_name);
    }
    sb_append(out, "\n");
}

static void append_array_defs(StringBuilder *out, const Schema *schema) {
    for (size_t i = 0; i < schema->array_count; ++i) {
        const char *element = schema->arrays[i].element_type;
        char array_name[256];
        make_array_name(array_name, sizeof(array_name), schema, element);
        char element_type_buf[256];
        const char *element_type = c_type_for(schema, element, element_type_buf, sizeof(element_type_buf), schema->arrays[i].line);
        sb_append(out,
            "struct %s {\n"
            "    %s *items;\n"
            "    uint32_t count;\n"
            "};\n\n",
            array_name, element_type);
    }
}

static void append_struct_defs(StringBuilder *out, const Schema *schema, int kind_filter) {
    char deprecated_macro[256];
    to_upper_str(deprecated_macro, sizeof(deprecated_macro), schema->prefix);
    strncat(deprecated_macro, "DEPRECATED", sizeof(deprecated_macro) - strlen(deprecated_macro) - 1);

    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        if (kind_filter != -1 && def->kind != (StructKind)kind_filter) { continue; }
        char struct_name[256];
        make_prefixed(struct_name, sizeof(struct_name), schema, def->name);
        sb_append(out, "struct %s {\n", struct_name);
        for (size_t j = 0; j < def->field_count; ++j) {
            const FieldDef *field = &def->fields[j];
            char buf[256];
            if (def->kind == STRUCT_KIND_MESSAGE) {
                sb_append(out, "    bool %sExist;\n", field->name);
            } else if (field->is_optional) {
                sb_append(out, "    bool %sExist;\n", field->name);
            }
            if (field->is_array) {
                char array_name[256];
                make_array_name(array_name, sizeof(array_name), schema, field->type_name);
                sb_append(out, "    %s %s", array_name, field->name);
                if (field->is_deprecated) { sb_append(out, " %s(\"deprecated\")", deprecated_macro); }
                sb_append(out, ";\n");
            } else {
                const char *ctype = c_type_for(schema, field->type_name, buf, sizeof(buf), field->line);
                sb_append(out, "    %s %s", ctype, field->name);
                if (field->bit_width > 0) {
                    sb_append(out, " : %u", field->bit_width);
                }
                if (field->is_deprecated) { sb_append(out, " %s(\"deprecated\")", deprecated_macro); }
                sb_append(out, ";\n");
            }
        }
        sb_append(out, "};\n\n");
    }
}

static void append_union_defs(StringBuilder *out, const Schema *schema) {
    for (size_t i = 0; i < schema->union_count; ++i) {
        const UnionDef *def = &schema->unions[i];
        char tag_enum[256];
        make_union_tag_enum_name(tag_enum, sizeof(tag_enum), schema, def);
        sb_append(out, "typedef enum %s {\n", tag_enum);
        for (size_t j = 0; j < def->field_count; ++j) {
            sb_append(out, "    %s_%s = %u,\n", tag_enum, def->fields[j].name, def->fields[j].field_id);
        }
        sb_append(out, "} %s;\n\n", tag_enum);

        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, def->name);
        sb_append(out, "struct %s {\n", union_name);
        sb_append(out, "    %s tag;\n", tag_enum);
        sb_append(out, "    union {\n");
        for (size_t j = 0; j < def->field_count; ++j) {
            char cbuf[256];
            const char *ctype = c_type_for(schema, def->fields[j].type_name, cbuf, sizeof(cbuf), def->fields[j].line);
            sb_append(out, "        %s %s;\n", ctype, def->fields[j].name);
        }
        sb_append(out, "    } value;\n");
        sb_append(out, "};\n\n");
    }
}

static void append_io_forward_decls(StringBuilder *out, const Schema *schema, const char *api_macro) {
    for (size_t i = 0; i < schema->array_count; ++i) {
        const char *element = schema->arrays[i].element_type;
        char array_name[256];
        make_array_name(array_name, sizeof(array_name), schema, element);
        sb_append(out, "%s bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema);\n",
            api_macro, array_name, array_name, schema->prefix, schema->prefix, schema->prefix);
        sb_append(out, "%s bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema);\n",
            api_macro, array_name, array_name, schema->prefix, schema->prefix);
    }
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        char struct_name[256];
        make_prefixed(struct_name, sizeof(struct_name), schema, def->name);
        sb_append(out, "%s bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema);\n",
            api_macro, struct_name, struct_name, schema->prefix, schema->prefix, schema->prefix);
        sb_append(out, "%s bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema);\n",
            api_macro, struct_name, struct_name, schema->prefix, schema->prefix);
    }
    for (size_t i = 0; i < schema->union_count; ++i) {
        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, schema->unions[i].name);
        sb_append(out, "%s bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema);\n",
            api_macro, union_name, union_name, schema->prefix, schema->prefix, schema->prefix);
        sb_append(out, "%s bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema);\n",
            api_macro, union_name, union_name, schema->prefix, schema->prefix);
    }
    if (schema->array_count || schema->struct_count) {
        sb_append(out, "\n");
    }
}

static void append_array_io(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    for (size_t i = 0; i < schema->array_count; ++i) {
        const char *element = schema->arrays[i].element_type;
        char array_name[256];
        make_array_name(array_name, sizeof(array_name), schema, element);
        char elem_type_buf[256];
        const char *elem_type = c_type_for(schema, element, elem_type_buf, sizeof(elem_type_buf), schema->arrays[i].line);
        bool elem_is_enum = type_is_enum(schema, element);
        const EnumDef *enum_def = find_enum_const(schema, element);
        const StructDef *elem_struct = find_struct_const(schema, element);
        char elem_struct_name[256] = {0};
        if (elem_struct) { make_prefixed(elem_struct_name, sizeof(elem_struct_name), schema, element); }
        
        sb_append(impl,
            "bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema) {\n"
            "    if (!memory) { return false; }\n"
            "    uint32_t count = 0;\n"
            "    if (!%sbuffer_read_u32(buffer, &count)) { return false; }\n"
            "    value->items = NULL;\n"
            "    value->count = 0;\n"
            "    if (count == 0) { return true; }\n"
            "    const uint32_t memory_marker = memory->used;\n"
            "    %s *items = (%s *)%sbuffer_push_aligned(memory, (size_t)count * sizeof(%s), (sizeof(%s) > sizeof(void *)) ? sizeof(%s) : sizeof(void *));\n"
            "    if (!items) { return false; }\n",
            array_name, array_name, schema->prefix, schema->prefix, schema->prefix, schema->prefix,
            elem_type, elem_type, schema->prefix, elem_type, elem_type, elem_type);
        sb_append(impl, "    for (uint32_t i = 0; i < count; ++i) {\n");
        if (elem_struct) {
            sb_append(impl,
                "        if (!%s_read(&items[i], buffer, memory, schema)) {\n"
                "            %sbuffer_pop_to(memory, memory_marker);\n"
                "            return false;\n"
                "        }\n",
                elem_struct_name, schema->prefix);
        } else if (elem_is_enum && enum_def) {
            char base_buf[256];
            const char *base_type = c_type_for(schema, enum_def->base_type, base_buf, sizeof(base_buf), enum_def->line);
            sb_append(impl, "        %s raw = 0;\n", base_type);
            sb_append(impl,
                "        if (!%sbuffer_read_%s(buffer, &raw)) {\n"
                "            %sbuffer_pop_to(memory, memory_marker);\n"
                "            return false;\n"
                "        }\n",
                schema->prefix, enum_def->base_type, schema->prefix);
            char struct_name[256];
            make_prefixed(struct_name, sizeof(struct_name), schema, element);
            sb_append(impl, "        items[i] = (%s)raw;\n", struct_name);
        } else {
            sb_append(impl,
                "        if (!%sbuffer_read_%s(buffer, &items[i])) {\n"
                "            %sbuffer_pop_to(memory, memory_marker);\n"
                "            return false;\n"
                "        }\n",
                schema->prefix, element, schema->prefix);
        }
        sb_append(impl,
            "    }\n"
            "    value->items = items;\n"
            "    value->count = count;\n"
            "    return true;\n"
            "}\n\n");
        sb_append(impl,
            "bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema) {\n"
            "    (void)schema;\n"
            "    if (!%sbuffer_write_u32(buffer, value->count)) { return false; }\n"
            "    for (uint32_t i = 0; i < value->count; ++i) {\n",
            array_name, array_name, schema->prefix, schema->prefix, schema->prefix);
        if (elem_struct) {
            sb_append(impl, "        if (!%s_write(&value->items[i], buffer, schema)) { return false; }\n", elem_struct_name);
        } else if (elem_is_enum && enum_def) {
            char base_buf[256];
            c_type_for(schema, enum_def->base_type, base_buf, sizeof(base_buf), enum_def->line);
            sb_append(impl, "        %s raw = (%s)value->items[i];\n", base_buf, base_buf);
            sb_append(impl, "        if (!%sbuffer_write_%s(buffer, raw)) { return false; }\n", schema->prefix, enum_def->base_type);
        } else {
            sb_append(impl, "        if (!%sbuffer_write_%s(buffer, value->items[i])) { return false; }\n", schema->prefix, element);
        }
        sb_append(impl,
            "    }\n"
            "    return true;\n"
            "}\n\n");
    }

    for (size_t i = 0; i < schema->union_count; ++i) {
        const UnionDef *def = &schema->unions[i];
        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, def->name);
        char tag_enum[256];
        make_union_tag_enum_name(tag_enum, sizeof(tag_enum), schema, def);
        char base_buf[256];
        const char *base_type = c_type_for(schema, def->base_type, base_buf, sizeof(base_buf), def->line);

        sb_append(impl, "bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema) {\n", union_name, union_name, schema->prefix, schema->prefix, schema->prefix);
        sb_append(impl, "    (void)schema;\n    if (!memory) { return false; }\n    memset(value, 0, sizeof(*value));\n");
        sb_append(impl, "    %s tag_raw = 0;\n", base_type);
        sb_append(impl, "    if (!%sbuffer_read_%s(buffer, &tag_raw)) { return false; }\n", schema->prefix, def->base_type);
        sb_append(impl, "    value->tag = (%s)tag_raw;\n", tag_enum);
        sb_append(impl, "    switch (value->tag) {\n");
        for (size_t j = 0; j < def->field_count; ++j) {
            const UnionField *field = &def->fields[j];
            sb_append(impl, "        case %s_%s: {\n", tag_enum, field->name);
            if (type_is_enum(schema, field->type_name)) {
                const EnumDef *en = find_enum_const(schema, field->type_name);
                char enum_base_buf[256];
                const char *enum_base = c_type_for(schema, en->base_type, enum_base_buf, sizeof(enum_base_buf), en->line);
                sb_append(impl, "            %s raw = 0;\n", enum_base);
                sb_append(impl, "            if (!%sbuffer_read_%s(buffer, &raw)) { return false; }\n", schema->prefix, en->base_type);
                sb_append(impl, "            value->value.%s = (%s)raw;\n", field->name, enum_base);
            } else if (is_scalar_type(schema, field->type_name)) {
                if (type_is_string(field->type_name)) {
                    sb_append(impl, "            if (!%sbuffer_read_string(buffer, memory, &value->value.%s)) { return false; }\n", schema->prefix, field->name);
                } else {
                    sb_append(impl, "            if (!%sbuffer_read_%s(buffer, &value->value.%s)) { return false; }\n", schema->prefix, field->type_name, field->name);
                }
            } else {
                char nested[256];
                make_prefixed(nested, sizeof(nested), schema, field->type_name);
                sb_append(impl, "            if (!%s_read(&value->value.%s, buffer, memory, schema)) { return false; }\n", nested, field->name);
            }
            sb_append(impl, "            return true;\n        }\n");
        }
        sb_append(impl, "        default: return false;\n    }\n");
        sb_append(impl, "}\n\n");

        sb_append(impl, "bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema) {\n", union_name, union_name, schema->prefix, schema->prefix);
        sb_append(impl, "    (void)schema;\n");
        sb_append(impl, "    if (!%sbuffer_write_%s(buffer, (%s)value->tag)) { return false; }\n", schema->prefix, def->base_type, base_type);
        sb_append(impl, "    switch (value->tag) {\n");
        for (size_t j = 0; j < def->field_count; ++j) {
            const UnionField *field = &def->fields[j];
            sb_append(impl, "        case %s_%s: {\n", tag_enum, field->name);
            if (type_is_enum(schema, field->type_name)) {
                const EnumDef *en = find_enum_const(schema, field->type_name);
                char enum_base_buf[256];
                const char *enum_base = c_type_for(schema, en->base_type, enum_base_buf, sizeof(enum_base_buf), en->line);
                sb_append(impl, "            if (!%sbuffer_write_%s(buffer, (%s)value->value.%s)) { return false; }\n", schema->prefix, en->base_type, enum_base, field->name);
            } else if (is_scalar_type(schema, field->type_name)) {
                if (type_is_string(field->type_name)) {
                    sb_append(impl, "            if (!%sbuffer_write_string(buffer, &value->value.%s)) { return false; }\n", schema->prefix, field->name);
                } else {
                    sb_append(impl, "            if (!%sbuffer_write_%s(buffer, value->value.%s)) { return false; }\n", schema->prefix, field->type_name, field->name);
                }
            } else {
                char nested[256];
                make_prefixed(nested, sizeof(nested), schema, field->type_name);
                sb_append(impl, "            if (!%s_write(&value->value.%s, buffer, schema)) { return false; }\n", nested, field->name);
            }
            sb_append(impl, "            return true;\n        }\n");
        }
        sb_append(impl, "        default: return false;\n    }\n");
        sb_append(impl, "}\n\n");
    }
}

static void append_struct_io(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    (void)decls;
    (void)api_macro;
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        char struct_name[256];
        make_prefixed(struct_name, sizeof(struct_name), schema, def->name);
        sb_append(impl, "bool %s_read(%s *value, %sBuffer *buffer, %sBuffer *memory, const %sSchemaInfo *schema) {\n", struct_name, struct_name, schema->prefix, schema->prefix, schema->prefix);
        sb_append(impl, "    if (!memory) { return false; }\n");
        sb_append(impl, "    memset(value, 0, sizeof(*value));\n");

        uint32_t optional_count = 0;
        for (size_t j = 0; j < def->field_count; ++j) {
            if (def->fields[j].is_optional) { optional_count++; }
        }

        if (optional_count > 0) {
            uint32_t bitmask_size = (optional_count + 7) / 8;
            sb_append(impl, "    uint8_t bitmask[%u];\n", bitmask_size);
            sb_append(impl, "    if (!%sbuffer_read_bytes(buffer, bitmask, sizeof(bitmask))) { return false; }\n", schema->prefix);
        }

        uint32_t opt_idx = 0;
        for (size_t j = 0; j < def->field_count; ++j) {
            const FieldDef *field = &def->fields[j];
            
            if (field->is_optional) {
                sb_append(impl, "    value->%sExist = (bitmask[%u] >> %u) & 1u;\n", field->name, opt_idx / 8, opt_idx % 8);
                sb_append(impl, "    if (value->%sExist) {\n", field->name);
                opt_idx++;
            }

            const char *indent = field->is_optional ? "        " : "    ";

            if (field->is_array) {
                char array_name[256];
                make_array_name(array_name, sizeof(array_name), schema, field->type_name);
                sb_append(impl, "%sif (!%s_read(&value->%s, buffer, memory, schema)) { return false; }\n", indent, array_name, field->name);
            } else if (type_is_enum(schema, field->type_name)) {
                const EnumDef *en = find_enum_const(schema, field->type_name);
                sb_append(impl, "%s{\n", indent);
                char base_buf[256];
                const char *base_type = c_type_for(schema, en->base_type, base_buf, sizeof(base_buf), en->line);
                sb_append(impl, "%s    %s raw = 0;\n", indent, base_type);
                sb_append(impl, "%s    if (!%sbuffer_read_%s(buffer, &raw)) { return false; }\n", indent, schema->prefix, en->base_type);
                char enum_name[256];
                make_prefixed(enum_name, sizeof(enum_name), schema, field->type_name);
                if (field->bit_width > 0) {
                    char target[256];
                    snprintf(target, sizeof(target), "value->%s", field->name);
                    char bw_indent[64];
                    snprintf(bw_indent, sizeof(bw_indent), "%s    ", indent);
                    append_bit_width_assign(impl, schema, field, "raw", target, bw_indent);
                } else {
                    sb_append(impl, "%s    value->%s = (%s)raw;\n", indent, field->name, enum_name);
                }
                sb_append(impl, "%s}\n", indent);
            } else if (is_scalar_type(schema, field->type_name)) {
                if (field->bit_width > 0) {
                    char buf[256];
                    const char *ctype = c_type_for(schema, field->type_name, buf, sizeof(buf), field->line);
                    sb_append(impl, "%s{\n", indent);
                    sb_append(impl, "%s    %s temp;\n", indent, ctype);
                    sb_append(impl, "%s    if (!%sbuffer_read_%s(buffer, &temp)) { return false; }\n", indent, schema->prefix, field->type_name);
                    char target[256];
                    snprintf(target, sizeof(target), "value->%s", field->name);
                    char bw_indent[64];
                    snprintf(bw_indent, sizeof(bw_indent), "%s    ", indent);
                    append_bit_width_assign(impl, schema, field, "temp", target, bw_indent);
                    sb_append(impl, "%s}\n", indent);
                } else {
                    if (type_is_string(field_base_type(schema, field))) {
                        sb_append(impl, "%sif (!%sbuffer_read_string(buffer, memory, &value->%s)) { return false; }\n", indent, schema->prefix, field->name);
                    } else {
                        sb_append(impl, "%sif (!%sbuffer_read_%s(buffer, &value->%s)) { return false; }\n", indent, schema->prefix, field->type_name, field->name);
                    }
                }
            } else {
                char nested[256];
                make_prefixed(nested, sizeof(nested), schema, field->type_name);
                sb_append(impl, "%sif (!%s_read(&value->%s, buffer, memory, schema)) { return false; }\n", indent, nested, field->name);
            }

            if (field->is_optional) {
                sb_append(impl, "    }\n");
            }
        }
        sb_append(impl, "    return true;\n");
        sb_append(impl, "}\n\n");

        sb_append(impl, "bool %s_write(const %s *value, %sBuffer *buffer, const %sSchemaInfo *schema) {\n", struct_name, struct_name, schema->prefix, schema->prefix);
        sb_append(impl, "    (void)schema;\n");
        
        optional_count = 0;
        for (size_t j = 0; j < def->field_count; ++j) {
            if (def->fields[j].is_optional) { optional_count++; }
        }

        if (optional_count > 0) {
            uint32_t bitmask_size = (optional_count + 7) / 8;
            sb_append(impl, "    uint8_t bitmask[%u];\n", bitmask_size);
            sb_append(impl, "    memset(bitmask, 0, sizeof(bitmask));\n");
            
            opt_idx = 0;
            for (size_t j = 0; j < def->field_count; ++j) {
                if (def->fields[j].is_optional) {
                    sb_append(impl, "    if (value->%sExist) { bitmask[%u] |= (1u << %u); }\n", def->fields[j].name, opt_idx / 8, opt_idx % 8);
                    opt_idx++;
                }
            }
            sb_append(impl, "    if (!%sbuffer_write_bytes(buffer, bitmask, sizeof(bitmask))) { return false; }\n", schema->prefix);
        }

        for (size_t j = 0; j < def->field_count; ++j) {
            const FieldDef *field = &def->fields[j];
            
            if (field->is_optional) {
                sb_append(impl, "    if (value->%sExist) {\n", field->name);
            }

            const char *indent = field->is_optional ? "        " : "    ";

            if (field->is_array) {
                char array_name[256];
                make_array_name(array_name, sizeof(array_name), schema, field->type_name);
                sb_append(impl, "%sif (!%s_write(&value->%s, buffer, schema)) { return false; }\n", indent, array_name, field->name);
            } else if (type_is_enum(schema, field->type_name)) {
                const EnumDef *en = find_enum_const(schema, field->type_name);
                char base_buf[256];
                const char *base_type = c_type_for(schema, en->base_type, base_buf, sizeof(base_buf), en->line);
                if (field->bit_width > 0) {
                    sb_append(impl, "%s{\n", indent);
                    char enum_name[256];
                    make_prefixed(enum_name, sizeof(enum_name), schema, field->type_name);
                    sb_append(impl, "%s    %s narrowed;\n", indent, enum_name);
                    char bw_indent[64];
                    snprintf(bw_indent, sizeof(bw_indent), "%s    ", indent);
                    char source[256];
                    snprintf(source, sizeof(source), "value->%s", field->name);
                    append_bit_width_assign(impl, schema, field, source, "narrowed", bw_indent);
                    sb_append(impl, "%s    if (!%sbuffer_write_%s(buffer, (%s)narrowed)) { return false; }\n", indent, schema->prefix, en->base_type, base_type);
                    sb_append(impl, "%s}\n", indent);
                } else {
                    sb_append(impl, "%sif (!%sbuffer_write_%s(buffer, (%s)value->%s)) { return false; }\n", indent, schema->prefix, en->base_type, base_type, field->name);
                }
            } else if (is_scalar_type(schema, field->type_name)) {
                if (field->bit_width > 0) {
                    char buf[256];
                    const char *ctype = c_type_for(schema, field->type_name, buf, sizeof(buf), field->line);
                    sb_append(impl, "%s{\n", indent);
                    sb_append(impl, "%s    %s temp;\n", indent, ctype);
                    char bw_indent[64];
                    snprintf(bw_indent, sizeof(bw_indent), "%s    ", indent);
                    char source[256];
                    snprintf(source, sizeof(source), "value->%s", field->name);
                    append_bit_width_assign(impl, schema, field, source, "temp", bw_indent);
                    sb_append(impl, "%s    if (!%sbuffer_write_%s(buffer, temp)) { return false; }\n", indent, schema->prefix, field->type_name);
                    sb_append(impl, "%s}\n", indent);
                } else {
                    if (type_is_string(field_base_type(schema, field))) {
                        sb_append(impl, "%sif (!%sbuffer_write_string(buffer, &value->%s)) { return false; }\n", indent, schema->prefix, field->name);
                    } else {
                        sb_append(impl, "%sif (!%sbuffer_write_%s(buffer, value->%s)) { return false; }\n", indent, schema->prefix, field->type_name, field->name);
                    }
                }
            } else {
                char nested[256];
                make_prefixed(nested, sizeof(nested), schema, field->type_name);
                sb_append(impl, "%sif (!%s_write(&value->%s, buffer, schema)) { return false; }\n", indent, nested, field->name);
            }

            if (field->is_optional) {
                sb_append(impl, "    }\n");
            }
        }
        sb_append(impl, "    return true;\n");
        sb_append(impl, "}\n\n");
    }
}

static void append_metadata(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro) {
    char type_id_name[256];
    make_prefixed(type_id_name, sizeof(type_id_name), schema, "type");
    sb_append(decls, "typedef enum %s {\n", type_id_name);
    for (size_t i = 0; i < schema->alias_count; ++i) {
        sb_append(decls, "    %s_%s,\n", type_id_name, schema->aliases[i].name);
    }
    for (size_t i = 0; i < schema->enum_count; ++i) {
        sb_append(decls, "    %s_%s,\n", type_id_name, schema->enums[i].name);
    }
    for (size_t i = 0; i < schema->struct_count; ++i) {
        sb_append(decls, "    %s_%s,\n", type_id_name, schema->structs[i].name);
    }
    for (size_t i = 0; i < schema->union_count; ++i) {
        sb_append(decls, "    %s_%s,\n", type_id_name, schema->unions[i].name);
    }
    for (size_t i = 0; i < schema->array_count; ++i) {
        sb_append(decls, "    %s_Array%s,\n", type_id_name, schema->arrays[i].element_type);
    }
    sb_append(decls, "} %s;\n\n", type_id_name);

    char param_info[256];
    make_prefixed(param_info, sizeof(param_info), schema, "ParameterInfo");
    sb_append(decls,
        "typedef struct %s {\n"
        "    const char *name;\n"
        "    uint32_t parameter_id;\n"
        "    %s type_id;\n"
        "    uint32_t offset;\n"
        "} %s;\n\n",
        param_info, type_id_name, param_info);

    char type_desc[256];
    make_prefixed(type_desc, sizeof(type_desc), schema, "TypeDescription");
    sb_append(decls,
        "typedef struct %s {\n"
        "    const char *name;\n"
        "    %s type_id;\n"
        "    size_t struct_size;\n"
        "    size_t no_padding_struct_size;\n"
        "    const %s *parameters;\n"
        "    uint32_t parameter_count;\n"
        "} %s;\n\n",
        type_desc, type_id_name, param_info, type_desc);

    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        char enum_name[256];
        snprintf(enum_name, sizeof(enum_name), "%s%sParameters", schema->prefix, def->name[0] ? (char[2]){(char)tolower((unsigned char)def->name[0]), '\0'} : "");
        char tmp[256];
        strcpy(tmp, def->name);
        tmp[0] = (char)tolower((unsigned char)tmp[0]);
        snprintf(enum_name, sizeof(enum_name), "%s%sParameters", schema->prefix, tmp);
        sb_append(decls, "typedef enum %s {\n", enum_name);
        for (size_t j = 0; j < def->field_count; ++j) {
            sb_append(decls, "    %s_%s = %u,\n", enum_name, def->fields[j].name, def->fields[j].field_id);
        }
        sb_append(decls, "} %s;\n\n", enum_name);

        char param_array[256];
        snprintf(param_array, sizeof(param_array), "%s%s_parameters", schema->prefix, def->name);
        sb_append(impl, "static const %s %s[] = {\n", param_info, param_array);
        for (size_t j = 0; j < def->field_count; ++j) {
            const FieldDef *field = &def->fields[j];
            char type_id_buf[256];
            snprintf(type_id_buf, sizeof(type_id_buf), "%s_%s", type_id_name, field->type_name);
            if (field->is_array) {
                snprintf(type_id_buf, sizeof(type_id_buf), "%s_Array%s", type_id_name, field->type_name);
            } else if (!find_alias_const(schema, field->type_name) && !find_enum_const(schema, field->type_name)) {
                snprintf(type_id_buf, sizeof(type_id_buf), "%s_%s", type_id_name, field->type_name);
            }
            char struct_name[256];
            make_prefixed(struct_name, sizeof(struct_name), schema, def->name);
            if (field->bit_width > 0) {
                sb_append(impl, "    { \"%s\", %s_%s, %s, UINT32_MAX },\n",
                    field->name, enum_name, field->name, type_id_buf);
            } else {
                sb_append(impl, "    { \"%s\", %s_%s, %s, offsetof(%s, %s) },\n",
                    field->name, enum_name, field->name, type_id_buf, struct_name, field->name);
            }
        }
        sb_append(impl, "};\n\n");
    }

    for (size_t i = 0; i < schema->union_count; ++i) {
        const UnionDef *def = &schema->unions[i];
        char lower[256];
        to_lower_first(lower, sizeof(lower), def->name);
        char enum_name[256];
        snprintf(enum_name, sizeof(enum_name), "%s%sParameters", schema->prefix, lower);
        sb_append(decls, "typedef enum %s {\n", enum_name);
        sb_append(decls, "    %s_tag = 0,\n", enum_name);
        for (size_t j = 0; j < def->field_count; ++j) {
            sb_append(decls, "    %s_%s = %u,\n", enum_name, def->fields[j].name, def->fields[j].field_id);
        }
        sb_append(decls, "} %s;\n\n", enum_name);

        char param_array[256];
        snprintf(param_array, sizeof(param_array), "%s%s_parameters", schema->prefix, def->name);
        sb_append(impl, "static const %s %s[] = {\n", param_info, param_array);
        char type_id_buf[256];
        snprintf(type_id_buf, sizeof(type_id_buf), "%s_%s", type_id_name, def->base_type);
        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, def->name);
        sb_append(impl, "    { \"tag\", %s_tag, %s, offsetof(%s, tag) },\n", enum_name, type_id_buf, union_name);
        for (size_t j = 0; j < def->field_count; ++j) {
            const UnionField *field = &def->fields[j];
            char field_type_buf[256];
            snprintf(field_type_buf, sizeof(field_type_buf), "%s_%s", type_id_name, field->type_name);
            if (!find_alias_const(schema, field->type_name) && !find_enum_const(schema, field->type_name) && !find_struct_const(schema, field->type_name) && !find_union_const(schema, field->type_name)) {
                snprintf(field_type_buf, sizeof(field_type_buf), "%s_%s", type_id_name, field->type_name);
            }
            sb_append(impl, "    { \"%s\", %s_%s, %s, offsetof(%s, value.%s) },\n",
                field->name, enum_name, field->name, field_type_buf, union_name, field->name);
        }
        sb_append(impl, "};\n\n");
    }

    sb_append(impl, "static const %s %stype_descriptions[] = {\n", type_desc, schema->prefix);
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        char struct_name[256];
        make_prefixed(struct_name, sizeof(struct_name), schema, def->name);
        char param_array[256];
        snprintf(param_array, sizeof(param_array), "%s%s_parameters", schema->prefix, def->name);
        sb_append(impl, "    { \"%s\", %s_%s, sizeof(%s), sizeof(%s), %s, %zu },\n",
            def->name, type_id_name, def->name, struct_name, struct_name, param_array, def->field_count);
    }
    for (size_t i = 0; i < schema->union_count; ++i) {
        const UnionDef *def = &schema->unions[i];
        char union_name[256];
        make_prefixed(union_name, sizeof(union_name), schema, def->name);
        char param_array[256];
        snprintf(param_array, sizeof(param_array), "%s%s_parameters", schema->prefix, def->name);
        size_t param_count = def->field_count + 1; // tag + variants
        sb_append(impl, "    { \"%s\", %s_%s, sizeof(%s), sizeof(%s), %s, %zu },\n",
            def->name, type_id_name, def->name, union_name, union_name, param_array, param_count);
    }
    sb_append(impl, "};\n\n");

    sb_append(decls, "%s const %s *%sget_type_description(%s type_id);\n", api_macro, type_desc, schema->prefix, type_id_name);
    sb_append(impl, "const %s *%sget_type_description(%s type_id) {\n", type_desc, schema->prefix, type_id_name);
    sb_append(impl, "    for (size_t i = 0; i < sizeof(%stype_descriptions)/sizeof(%stype_descriptions[0]); ++i) {\n", schema->prefix, schema->prefix);
    sb_append(impl, "        if (%stype_descriptions[i].type_id == type_id) { return &%stype_descriptions[i]; }\n", schema->prefix, schema->prefix);
    sb_append(impl, "    }\n    return NULL;\n}\n");
}

static void generate_header(const Schema *schema, const char *input_path, const char *output_path, const StringBuilder *bin_data) {
    StringBuilder out;
    StringBuilder impl;
    sb_init(&out);
    sb_init(&impl);
    char prefix_upper[256];
    to_upper_str(prefix_upper, sizeof(prefix_upper), schema->prefix);
    char guard[256];
    snprintf(guard, sizeof(guard), "%s_H_INCLUDE", prefix_upper);
    char api_macro[256];
    snprintf(api_macro, sizeof(api_macro), "%s_API", prefix_upper);
    char static_macro[256];
    snprintf(static_macro, sizeof(static_macro), "%s_STATIC", prefix_upper);
    char impl_macro[256];
    snprintf(impl_macro, sizeof(impl_macro), "%s_IMPLEMENTATION", prefix_upper);

    sb_append(&out, "// Generated by schema_gen from %s\n", input_path);
    sb_append(&out, "#ifndef %s\n#define %s\n\n", guard, guard);
    sb_append(&out, "// To create the implementation, define %s before including this file in one translation unit.\n\n", impl_macro);
    sb_append(&out, "#include <stdbool.h>\n#include <stddef.h>\n#include <stdint.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n#include <float.h>\n\n");
    sb_append(&out, "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n");
    sb_append(&out,
        "#ifndef %s\n"
        "#  ifdef %s\n"
        "#    define %s static\n"
        "#  else\n"
        "#    define %s extern\n"
        "#  endif\n"
        "#endif\n\n",
        api_macro, static_macro, api_macro, api_macro);

    sb_append(&out,
        "#ifndef %sDEPRECATED\n"
        "#  if defined(__GNUC__) || defined(__clang__)\n"
        "#    define %sDEPRECATED(msg) __attribute__((deprecated(msg)))\n"
        "#  else\n"
        "#    define %sDEPRECATED(msg)\n"
        "#  endif\n"
        "#endif\n\n",
        prefix_upper, prefix_upper, prefix_upper);

    sb_append(&out,
        "#if defined(__BYTE_ORDER__) && defined(__ORDER_BIG_ENDIAN__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__\n"
        "#  define %s_IS_BIG_ENDIAN 1\n"
        "#elif defined(__BIG_ENDIAN__)\n"
        "#  define %s_IS_BIG_ENDIAN 1\n"
        "#else\n"
        "#  define %s_IS_BIG_ENDIAN 0\n"
        "#endif\n\n"
        "#if %s_IS_BIG_ENDIAN\n"
        "#  ifdef _MSC_VER\n"
        "#    include <stdlib.h>\n"
        "#    define %s_bswap16(x) _byteswap_ushort(x)\n"
        "#    define %s_bswap32(x) _byteswap_ulong(x)\n"
        "#    define %s_bswap64(x) _byteswap_uint64(x)\n"
        "#  elif defined(__GNUC__) || defined(__clang__)\n"
        "#    define %s_bswap16(x) __builtin_bswap16(x)\n"
        "#    define %s_bswap32(x) __builtin_bswap32(x)\n"
        "#    define %s_bswap64(x) __builtin_bswap64(x)\n"
        "#  else\n"
        "     static inline uint16_t %s_bswap16(uint16_t x) { return (x >> 8) | (x << 8); }\n"
        "     static inline uint32_t %s_bswap32(uint32_t x) { return ((x >> 24) & 0xff) | ((x >> 8) & 0xff00) | ((x << 8) & 0xff0000) | ((x << 24) & 0xff000000); }\n"
        "     static inline uint64_t %s_bswap64(uint64_t x) { return ((x >> 56) & 0xff) | ((x >> 40) & 0xff00) | ((x >> 24) & 0xff0000) | ((x >> 8) & 0xff000000) | ((x << 8) & 0xff00000000) | ((x << 24) & 0xff0000000000) | ((x << 40) & 0xff000000000000) | ((x << 56) & 0xff00000000000000); }\n"
        "#  endif\n"
        "#endif\n\n",
        schema->prefix, schema->prefix, schema->prefix, schema->prefix,
        schema->prefix, schema->prefix, schema->prefix,
        schema->prefix, schema->prefix, schema->prefix,
        schema->prefix, schema->prefix, schema->prefix);

    append_memory_buffer(&out, &impl, schema, api_macro);
    append_buffer_helpers(&out, &impl, schema, api_macro);
    append_wire_helpers(&out, &impl, schema, api_macro);
    append_string_helpers(&out, &impl, schema, api_macro);
    append_compact_helpers(&out, &impl, schema, api_macro);
    append_struct_forward(&out, schema);
    append_array_forward(&out, schema);
    append_enum_defs(&out, schema);
    append_array_defs(&out, schema);
    append_struct_defs(&out, schema, STRUCT_KIND_STRUCT);
    append_union_defs(&out, schema);
    append_struct_defs(&out, schema, STRUCT_KIND_MESSAGE);
    append_io_forward_decls(&out, schema, api_macro);
    append_array_io(&out, &impl, schema, api_macro);
    append_struct_io(&out, &impl, schema, api_macro);
    append_metadata(&out, &impl, schema, api_macro);
    append_compact_codecs(&out, &impl, schema, api_macro);
    append_runtime_schema_defs(&out, &impl, schema, api_macro, bin_data);

    sb_append(&out, "#ifdef __cplusplus\n}\n#endif\n\n#endif /* %s */\n", guard);

    if (impl.count) {
        sb_append(&out, "\n#ifdef %s\n\n", impl_macro);
        sb_append_bytes(&out, impl.data, impl.count);
        sb_append(&out, "#endif /* %s */\n", impl_macro);
    }

    FILE *f = fopen(output_path, "wb");
    if (!f) { fatal("schema_gen: failed to open '%s' for writing", output_path); }
    fwrite(out.data, 1, out.count, f);
    fclose(f);
    free(out.data);
    free(impl.data);
}

// MARK: Binary schema generation

#define BINARY_MAGIC "BKIW"
#define BINARY_VERSION 4

static void sb_write_u8(StringBuilder *sb, uint8_t v) {
    sb_append_bytes(sb, (char *)&v, 1);
}

static void sb_write_varuint(StringBuilder *sb, uint64_t v) {
    do {
        uint8_t byte = (uint8_t)(v & 0x7F);
        v >>= 7;
        if (v) { byte |= 0x80; }
        sb_write_u8(sb, byte);
    } while (v);
}

static void sb_write_strz(StringBuilder *sb, const char *s) {
    if (!s) { sb_write_u8(sb, 0); return; }
    sb_append_bytes(sb, s, strlen(s));
    sb_write_u8(sb, 0);
}

static uint32_t get_type_id(const Schema *schema, const char *name) {
    // 1. Aliases (Builtins)
    for (size_t i = 0; i < schema->alias_count; ++i) {
        if (strcmp(schema->aliases[i].name, name) == 0) { return (uint32_t)i; }
    }
    // 2. Enums
    for (size_t i = 0; i < schema->enum_count; ++i) {
        if (strcmp(schema->enums[i].name, name) == 0) { return (uint32_t)(schema->alias_count + i); }
    }
    // 3. Structs/Messages
    for (size_t i = 0; i < schema->struct_count; ++i) {
        if (strcmp(schema->structs[i].name, name) == 0) { return (uint32_t)(schema->alias_count + schema->enum_count + i); }
    }
    // 4. Unions
    for (size_t i = 0; i < schema->union_count; ++i) {
        if (strcmp(schema->unions[i].name, name) == 0) { return (uint32_t)(schema->alias_count + schema->enum_count + schema->struct_count + i); }
    }
    
    // Check if it's an array type name
    for (size_t i = 0; i < schema->array_count; ++i) {
        char array_name[256];
        make_array_name(array_name, sizeof(array_name), schema, schema->arrays[i].element_type);
        if (strcmp(array_name, name) == 0) {
            return get_type_id(schema, schema->arrays[i].element_type);
        }
    }

    fatal("schema_gen: unknown type '%s' for binary schema", name);
    return 0;
}

static void generate_binary_schema_data(const Schema *schema, StringBuilder *sb) {
    // Header
    sb_append_bytes(sb, BINARY_MAGIC, 4);
    sb_write_u8(sb, BINARY_VERSION);

    // Calculate total types explicitly emitted
    uint32_t type_count = (uint32_t)(schema->enum_count + schema->struct_count + schema->union_count);
    sb_write_varuint(sb, type_count);

    // Type Definitions
    // 1. Enums
    for (size_t i = 0; i < schema->enum_count; ++i) {
        const EnumDef *def = &schema->enums[i];
        sb_write_strz(sb, def->name);
        sb_write_u8(sb, 0); // 0=ENUM
        sb_write_varuint(sb, get_type_id(schema, def->name));
        sb_write_varuint(sb, 0); // field_count
    }

    // 2. Structs/Messages
    for (size_t i = 0; i < schema->struct_count; ++i) {
        const StructDef *def = &schema->structs[i];
        sb_write_strz(sb, def->name);
        sb_write_u8(sb, def->kind == STRUCT_KIND_MESSAGE ? 2 : 1); // 1=STRUCT, 2=MESSAGE
        sb_write_varuint(sb, get_type_id(schema, def->name));
        sb_write_varuint(sb, def->field_count + def->removed_count);

        for (size_t j = 0; j < def->field_count; ++j) {
            const FieldDef *field = &def->fields[j];
            sb_write_strz(sb, field->name);
            sb_write_varuint(sb, field->field_id);
            sb_write_varuint(sb, get_type_id(schema, field->type_name));
            
            uint8_t flags = 0;
            if (field->is_array) { flags |= 1; }
            if (field->is_optional) { flags |= 2; }
            if (field->is_deprecated) { flags |= 4; }
            if (field->is_removed) { flags |= 8; }
            sb_write_u8(sb, flags);
            sb_write_varuint(sb, field->bit_width);
        }
        for (size_t j = 0; j < def->removed_count; ++j) {
            const FieldDef *field = &def->removed_fields[j];
            sb_write_strz(sb, field->name ? field->name : "");
            sb_write_varuint(sb, field->field_id);
            sb_write_varuint(sb, get_type_id(schema, field->type_name));
            
            uint8_t flags = 8; // removed/reserved marker
            sb_write_u8(sb, flags);
            sb_write_varuint(sb, field->bit_width);
        }
    }

    // 3. Unions
    for (size_t i = 0; i < schema->union_count; ++i) {
        const UnionDef *def = &schema->unions[i];
        sb_write_strz(sb, def->name);
        sb_write_u8(sb, 3); // 3=UNION
        sb_write_varuint(sb, get_type_id(schema, def->name));
        sb_write_varuint(sb, get_type_id(schema, def->base_type)); // tag base type id
        sb_write_varuint(sb, def->field_count);
        for (size_t j = 0; j < def->field_count; ++j) {
            const UnionField *field = &def->fields[j];
            sb_write_strz(sb, field->name);
            sb_write_varuint(sb, field->field_id);
            sb_write_varuint(sb, get_type_id(schema, field->type_name));
            sb_write_u8(sb, 0); // flags
            sb_write_varuint(sb, 0); // bit_width
        }
    }
}

// --- Compatibility checking ---

static const char *compat_find_type_name_by_id(const CompatSchema *cs, size_t builtin_alias_count, uint32_t type_id) {
    for (size_t i = 0; i < cs->type_count; ++i) {
        if (cs->types[i].type_id == type_id) { return cs->types[i].name; }
    }
    if (type_id < builtin_alias_count) { return BUILTIN_ALIASES[type_id].name; }
    return NULL;
}

static const char *compat_read_strz(const uint8_t **cursor, const uint8_t *end) {
    const char *str = (const char *)*cursor;
    while (*cursor < end && **cursor != 0) { (*cursor)++; }
    if (*cursor < end) { (*cursor)++; return str; }
    return NULL;
}

static uint64_t compat_read_varuint(const uint8_t **cursor, const uint8_t *end) {
    uint64_t result = 0;
    uint32_t shift = 0;
    while (*cursor < end && shift < 64) {
        uint8_t byte = *(*cursor)++;
        result |= ((uint64_t)(byte & 0x7F) << shift);
        if ((byte & 0x80) == 0) { break; }
        shift += 7;
    }
    return result;
}

static void compat_parse_binary_schema(const char *path, size_t builtin_alias_count, CompatSchema *out) {
    memset(out, 0, sizeof(*out));
    (void)builtin_alias_count;
    size_t len = 0;
    char *data = read_entire_file(path, &len);
    const uint8_t *cursor = (const uint8_t *)data;
    const uint8_t *end = cursor + len;
    if (len < 5 || memcmp(cursor, BINARY_MAGIC, 4) != 0) {
        free(data);
        fatal("schema_gen: existing binary schema '%s' has invalid magic", path);
    }
    cursor += 4;
    uint8_t version = *cursor++;
    if (version != BINARY_VERSION && version != 2 && version != 3) {
        free(data);
        fatal("schema_gen: existing binary schema '%s' has incompatible version (%u)", path, (unsigned)version);
    }
    uint64_t type_count = compat_read_varuint(&cursor, end);
    out->types = (CompatType *)calloc(type_count, sizeof(CompatType));
    if (!out->types) { free(data); fatal("schema_gen: out of memory while reading existing schema"); }
    out->type_count = (size_t)type_count;
    for (size_t i = 0; i < out->type_count; ++i) {
        CompatType *t = &out->types[i];
        const char *name = compat_read_strz(&cursor, end);
        if (!name) { free(data); fatal("schema_gen: malformed string in existing schema"); }
        t->name = str_dup(name);
        if (cursor >= end) { free(data); fatal("schema_gen: truncated schema while reading kind"); }
        t->kind = *cursor++;
        t->type_id = (uint32_t)compat_read_varuint(&cursor, end);
        if (t->kind == 3 && version >= 4) {
            t->union_base_type_id = (uint32_t)compat_read_varuint(&cursor, end);
        } else {
            t->union_base_type_id = 0;
        }
        uint64_t fcount = compat_read_varuint(&cursor, end);
        t->field_count = (uint32_t)fcount;
        if (fcount > 0) {
            t->fields = (CompatField *)calloc(fcount, sizeof(CompatField));
            if (!t->fields) { free(data); fatal("schema_gen: out of memory while reading fields"); }
            for (uint32_t j = 0; j < t->field_count; ++j) {
                CompatField *f = &t->fields[j];
                const char *fname = compat_read_strz(&cursor, end);
                if (!fname) { free(data); fatal("schema_gen: malformed field name"); }
                f->name = str_dup(fname);
                f->id = (uint32_t)compat_read_varuint(&cursor, end);
                f->type_id = (uint32_t)compat_read_varuint(&cursor, end);
                if (cursor >= end) { free(data); fatal("schema_gen: truncated while reading field flags"); }
                uint8_t flags = *cursor++;
                f->is_array = (flags & 1) != 0;
                f->is_optional = (flags & 2) != 0;
                f->is_deprecated = (flags & 4) != 0;
                f->is_removed = (flags & 8) != 0;
                f->bit_width = (uint32_t)compat_read_varuint(&cursor, end);
            }
        }
    }
    free(data);
}

static void check_backward_compatibility(const char *existing_path, const Schema *schema) {
    CompatSchema old_schema = {0};
    compat_parse_binary_schema(existing_path, schema->builtin_alias_count, &old_schema);

    for (size_t i = 0; i < old_schema.type_count; ++i) {
        const CompatType *ot = &old_schema.types[i];
        if (ot->kind == 0) {
            if (!find_enum_const(schema, ot->name)) {
                fatal("%s: incompatible change: removed enum '%s'", schema->filename, ot->name);
            }
            continue;
        }
        if (ot->kind == 3) { // UNION
            const UnionDef *un = find_union_const(schema, ot->name);
            if (!un) { fatal("%s: incompatible change: removed union '%s'", schema->filename, ot->name); }
            uint32_t new_base_id = get_type_id(schema, un->base_type);
            if (new_base_id != ot->union_base_type_id) {
                const char *old_base = compat_find_type_name_by_id(&old_schema, schema->builtin_alias_count, ot->union_base_type_id);
                fatal("%s:%zu: incompatible change: union '%s' tag type changed (was '%s')", schema->filename, un->line, un->name, old_base ? old_base : "(unknown)");
            }
            if (un->field_count != ot->field_count) {
                fatal("%s:%zu: incompatible change: union '%s' variant count changed (was %u, now %zu)", schema->filename, un->line, un->name, ot->field_count, un->field_count);
            }
            for (size_t j = 0; j < ot->field_count; ++j) {
                const CompatField *of = &ot->fields[j];
                const UnionField *nf = NULL;
                for (size_t k = 0; k < un->field_count; ++k) {
                    if (un->fields[k].field_id == of->id) { nf = &un->fields[k]; break; }
                }
                if (!nf) {
                    fatal("%s:%zu: incompatible change: removed union variant id %u from '%s'", schema->filename, un->line, of->id, un->name);
                }
                if (strcmp(nf->name, of->name) != 0) {
                    fatal("%s:%zu: incompatible change: union variant id %u renamed from '%s' to '%s'", schema->filename, un->line, of->id, of->name, nf->name);
                }
                const char *old_type_name = compat_find_type_name_by_id(&old_schema, schema->builtin_alias_count, of->type_id);
                if (!old_type_name) {
                    fatal("%s:%zu: incompatible change: union variant '%s' refers to unknown type id %u in existing schema", schema->filename, un->line, nf->name, of->type_id);
                }
                uint32_t new_old_type_id = get_type_id(schema, old_type_name);
                uint32_t new_field_type_id = get_type_id(schema, nf->type_name);
                if (new_old_type_id != new_field_type_id) {
                    fatal("%s:%zu: incompatible change: union variant '%s' type changed (was '%s')", schema->filename, un->line, nf->name, old_type_name);
                }
            }
            continue;
        }
        StructKind expected_kind = (ot->kind == 2) ? STRUCT_KIND_MESSAGE : STRUCT_KIND_STRUCT;
        const StructDef *st = find_struct_const(schema, ot->name);
        if (!st) { fatal("%s: incompatible change: removed type '%s'", schema->filename, ot->name); }
        if (st->kind != expected_kind) { fatal("%s:%zu: incompatible change: kind of '%s' changed", schema->filename, st->line, ot->name); }
        if (st->kind == STRUCT_KIND_MESSAGE) {
            for (size_t j = 0; j < ot->field_count; ++j) {
                const CompatField *of = &ot->fields[j];
                if (of->is_removed) {
                    if (struct_find_field_by_id(st, of->id)) {
                        fatal("%s:%zu: incompatible change: field id %u in message '%s' was previously removed", schema->filename, st->line, of->id, st->name);
                    }
                    if (!struct_has_removed_id(st, of->id)) {
                        fatal("%s:%zu: incompatible change: removed field id %u from message '%s' is no longer marked removed", schema->filename, st->line, of->id, st->name);
                    }
                    continue;
                }
                const FieldDef *nf = struct_find_field_by_id(st, of->id);
                if (!nf) {
                    const FieldDef *by_name = struct_find_field_by_name(st, of->name);
                    if (by_name) {
                        fatal("%s:%zu: incompatible change: field '%s' changed id (%u -> %u)",
                              schema->filename, by_name->line, of->name, of->id, by_name->field_id);
                    }
                    if (struct_has_removed_id(st, of->id)) {
                        continue; // intentionally removed
                    }
                    fatal("%s:%zu: incompatible change: field '%s' (id %u) removed from message '%s' without reserving the id",
                          schema->filename, st->line, of->name, of->id, st->name);
                }
                if (strcmp(nf->name, of->name) != 0) {
                    fatal("%s:%zu: incompatible change: field id %u in message '%s' renamed from '%s' to '%s'",
                          schema->filename, nf->line, of->id, st->name, of->name, nf->name);
                }
                if (nf->is_array != of->is_array || nf->is_optional != of->is_optional || nf->is_deprecated != of->is_deprecated) {
                    fatal("%s:%zu: incompatible change: field '%s' modifiers changed", schema->filename, nf->line, nf->name);
                }
                if (nf->bit_width != of->bit_width) {
                    fatal("%s:%zu: incompatible change: field '%s' bit width changed (%u -> %u)", schema->filename, nf->line, nf->name, of->bit_width, nf->bit_width);
                }
                const char *old_type_name = compat_find_type_name_by_id(&old_schema, schema->builtin_alias_count, of->type_id);
                if (!old_type_name) {
                    fatal("%s:%zu: incompatible change: field '%s' refers to unknown type id %u in existing schema", schema->filename, nf->line, nf->name, of->type_id);
                }
                uint32_t new_old_type_id = get_type_id(schema, old_type_name);
                uint32_t new_field_type_id = get_type_id(schema, nf->type_name);
                if (new_old_type_id != new_field_type_id) {
                    fatal("%s:%zu: incompatible change: field '%s' type changed (was '%s')", schema->filename, nf->line, nf->name, old_type_name);
                }
            }
        } else {
            if (st->field_count != ot->field_count) {
                if (st->field_count < ot->field_count) {
                    const CompatField *of = &ot->fields[st->field_count];
                    fatal("%s:%zu: incompatible change: removed field '%s' from struct '%s' (struct layouts are fixed)",
                          schema->filename, st->line, of->name, ot->name);
                } else {
                    const FieldDef *nf = &st->fields[ot->field_count];
                    fatal("%s:%zu: incompatible change: added field '%s' to struct '%s' (struct layouts are fixed)",
                          schema->filename, nf->line, nf->name, ot->name);
                }
            }
            for (size_t j = 0; j < st->field_count; ++j) {
                const FieldDef *nf = &st->fields[j];
                const CompatField *of = &ot->fields[j];
                if (strcmp(nf->name, of->name) != 0) {
                    fatal("%s:%zu: incompatible change: field %zu in '%s' missing expected '%s' (found '%s'; removing/reordering fields breaks binary compatibility)",
                          schema->filename, nf->line, j, ot->name, of->name, nf->name);
                }
                if (!nf->has_field_id || nf->field_id != of->id) {
                    fatal("%s:%zu: incompatible change: field '%s' id changed (%u -> %u)", schema->filename, nf->line, nf->name, of->id, nf->has_field_id ? nf->field_id : UINT32_MAX);
                }
                if (nf->is_array != of->is_array || nf->is_optional != of->is_optional || nf->is_deprecated != of->is_deprecated) {
                    fatal("%s:%zu: incompatible change: field '%s' modifiers changed", schema->filename, nf->line, nf->name);
                }
                if (nf->bit_width != of->bit_width) {
                    fatal("%s:%zu: incompatible change: field '%s' bit width changed (%u -> %u)", schema->filename, nf->line, nf->name, of->bit_width, nf->bit_width);
                }
                const char *old_type_name = compat_find_type_name_by_id(&old_schema, schema->builtin_alias_count, of->type_id);
                if (!old_type_name) {
                    fatal("%s:%zu: incompatible change: field '%s' refers to unknown type id %u in existing schema", schema->filename, nf->line, nf->name, of->type_id);
                }
                uint32_t new_old_type_id = get_type_id(schema, old_type_name);
                uint32_t new_field_type_id = get_type_id(schema, nf->type_name);
                if (new_old_type_id != new_field_type_id) {
                    fatal("%s:%zu: incompatible change: field '%s' type changed (was '%s')", schema->filename, nf->line, nf->name, old_type_name);
                }
            }
        }
    }
}

// MARK: Runtime schema support

static void append_runtime_schema_defs(StringBuilder *decls, StringBuilder *impl, const Schema *schema, const char *api_macro, const StringBuilder *bin_data) {
    // Embed binary schema
    sb_append(impl, "#define %sBINARY_MAGIC \"BKIW\"\n", schema->prefix);
    sb_append(impl, "#define %sBINARY_VERSION %d\n\n", schema->prefix, BINARY_VERSION);
    
    sb_append(impl,
        "const uint8_t %sschema_blob[] = {\n",
        schema->prefix);
    for (size_t i = 0; i < bin_data->count; ++i) {
        if (i % 16 == 0) { sb_append(impl, "\n    "); }
        sb_append(impl, "0x%02x, ", (unsigned char)bin_data->data[i]);
    }
    sb_append(impl, "\n};\n\n");

    // Runtime Schema Structures
    sb_append(decls,
        "typedef struct %sSchemaField {\n"
        "    const char *name;\n"
        "    uint32_t id;\n"
        "    uint32_t type_id;\n"
        "    uint32_t bit_width;\n"
        "    bool is_array;\n"
        "    bool is_optional;\n"
        "    bool is_deprecated;\n"
        "    bool is_removed;\n"
        "    int32_t mapping;\n"
        "} %sSchemaField;\n\n",
        schema->prefix, schema->prefix);

    sb_append(decls,
        "typedef struct %sSchemaType {\n"
        "    const char *name;\n"
        "    uint8_t kind; // 0=ENUM, 1=STRUCT, 2=MESSAGE, 3=UNION\n"
        "    uint32_t type_id;\n"
        "    uint32_t tag_type_id; // only for unions\n"
        "    %sSchemaField *fields;\n"
        "    uint32_t field_count;\n"
        "} %sSchemaType;\n\n",
        schema->prefix, schema->prefix, schema->prefix);

    sb_append(decls,
        "struct %sSchemaInfo {\n"
        "    uint8_t version;\n"
        "    %sSchemaType *types;\n"
        "    uint32_t type_count;\n"
        "};\n\n",
        schema->prefix, schema->prefix);

    // Parse Function Declaration
    sb_append(decls, "%s const %sSchemaInfo *%sparse_schema(const uint8_t *data, size_t size, %sBuffer *allocator);\n",
        api_macro, schema->prefix, schema->prefix, schema->prefix);
    
    sb_append(decls, "%s const uint8_t *%sget_schema_blob(size_t *out_size);\n", api_macro, schema->prefix);
    sb_append(decls, "%s const %sSchemaInfo *%sget_embedded_schema(%sBuffer *allocator);\n",
        api_macro, schema->prefix, schema->prefix, schema->prefix);
    
    // Skip Function Declaration
    sb_append(decls, "%s bool %sskip_generic(%sBuffer *buffer, uint32_t type_id, bool is_array, const %sSchemaInfo *schema);\n",
        api_macro, schema->prefix, schema->prefix, schema->prefix);

    // Parse Function Implementation
    sb_append(impl,
        "static const char *%sread_strz(const uint8_t **cursor, const uint8_t *end) {\n"
        "    const char *str = (const char *)*cursor;\n"
        "    while (*cursor < end && **cursor != 0) { (*cursor)++; }\n"
        "    if (*cursor < end) { (*cursor)++; return str; }\n"
        "    return \"\";\n"
        "}\n\n", schema->prefix);

    sb_append(impl,
        "static uint64_t %sread_varuint(const uint8_t **cursor, const uint8_t *end) {\n"
        "    uint64_t result = 0;\n"
        "    uint32_t shift = 0;\n"
        "    while (*cursor < end) {\n"
        "        uint8_t byte = *(*cursor)++;\n"
        "        result |= ((uint64_t)(byte & 0x7F) << shift);\n"
        "        if ((byte & 0x80) == 0) { break; }\n"
        "        shift += 7;\n"
        "    }\n"
        "    return result;\n"
        "}\n\n", schema->prefix);

    sb_append(impl,
        "const %sSchemaInfo *%sparse_schema(const uint8_t *data, size_t size, %sBuffer *allocator) {\n"
        "    if (!data || !allocator) return NULL;\n"
        "    const uint8_t *cursor = data;\n"
        "    const uint8_t *end = data + size;\n"
        "    if (size < 5 || memcmp(cursor, %sBINARY_MAGIC, 4) != 0) return NULL;\n"
        "    cursor += 4;\n"
        "    uint8_t version = *cursor++;\n"
        "    if (version != %sBINARY_VERSION && version != 2 && version != 3) return NULL;\n"
        "    \n"
        "    uint64_t count = %sread_varuint(&cursor, end);\n"
        "    %sSchemaInfo *info = (%sSchemaInfo *)%sbuffer_push_aligned(allocator, sizeof(%sSchemaInfo), sizeof(void*));\n"
        "    if (!info) return NULL;\n"
        "    info->version = version;\n"
        "    info->type_count = (uint32_t)count;\n"
        "    info->types = (%sSchemaType *)%sbuffer_push_aligned(allocator, sizeof(%sSchemaType) * count, sizeof(void*));\n"
        "    if (!info->types) return NULL;\n"
        "    \n"
        "    for (uint32_t i = 0; i < count; ++i) {\n"
        "        %sSchemaType *type = &info->types[i];\n"
        "        type->name = %sread_strz(&cursor, end);\n"
        "        type->kind = *cursor++;\n"
        "        type->type_id = (uint32_t)%sread_varuint(&cursor, end);\n"
        "        if (type->kind == 3 && version >= 4) {\n"
        "            type->tag_type_id = (uint32_t)%sread_varuint(&cursor, end);\n"
        "        } else {\n"
        "            type->tag_type_id = 0;\n"
        "        }\n"
        "        uint64_t fcount = %sread_varuint(&cursor, end);\n"
        "        type->field_count = (uint32_t)fcount;\n"
        "        if (fcount > 0) {\n"
        "            type->fields = (%sSchemaField *)%sbuffer_push_aligned(allocator, sizeof(%sSchemaField) * fcount, sizeof(void*));\n"
        "            if (!type->fields) return NULL;\n"
        "            for (uint32_t j = 0; j < fcount; ++j) {\n"
        "                type->fields[j].name = %sread_strz(&cursor, end);\n"
        "                type->fields[j].id = (uint32_t)%sread_varuint(&cursor, end);\n"
        "                type->fields[j].type_id = (uint32_t)%sread_varuint(&cursor, end);\n"
        "                uint8_t flags = *cursor++;\n"
        "                type->fields[j].bit_width = (uint32_t)%sread_varuint(&cursor, end);\n"
        "                type->fields[j].is_array = (flags & 1) != 0;\n"
        "                type->fields[j].is_optional = (flags & 2) != 0;\n"
        "                type->fields[j].is_deprecated = (flags & 4) != 0;\n"
        "                type->fields[j].is_removed = (flags & 8) != 0;\n"
        "                type->fields[j].mapping = -1;\n"
        "            }\n"
        "        } else {\n"
        "            type->fields = NULL;\n"
        "        }\n"
        "    }\n"
        "    \n"
        "    // Resolve schema against local types\n"
        "    for (uint32_t i = 0; i < info->type_count; ++i) {\n"
        "        %sSchemaType *type = &info->types[i];\n"
        "        const %sTypeDescription *desc = NULL;\n"
        "        for (size_t j = 0; j < sizeof(%stype_descriptions)/sizeof(%stype_descriptions[0]); ++j) {\n"
        "            if (strcmp(%stype_descriptions[j].name, type->name) == 0) {\n"
        "                desc = &%stype_descriptions[j];\n"
        "                break;\n"
        "            }\n"
        "        }\n"
        "        if (!desc) continue;\n"
        "        \n"
        "        for (uint32_t j = 0; j < type->field_count; ++j) {\n"
        "            %sSchemaField *field = &type->fields[j];\n"
        "            if (field->is_removed) continue;\n"
        "            for (uint32_t k = 0; k < desc->parameter_count; ++k) {\n"
        "                if (strcmp(desc->parameters[k].name, field->name) == 0) {\n"
        "                    field->mapping = (int32_t)desc->parameters[k].parameter_id;\n"
        "                    break;\n"
        "                }\n"
        "            }\n"
        "        }\n"
        "    }\n"
        "    return info;\n"
        "}\n\n",
        schema->prefix, // SchemaInfo
        schema->prefix, // parse_schema name
        schema->prefix, // Buffer
        schema->prefix, // BINARY_MAGIC
        schema->prefix, // BINARY_VERSION
        schema->prefix, // read_varuint
        schema->prefix, schema->prefix, schema->prefix, schema->prefix, // SchemaInfo allocation
        schema->prefix, schema->prefix, schema->prefix, // types allocation
        schema->prefix, // SchemaType reference
        schema->prefix, // read_strz
        schema->prefix, // type_id varuint
        schema->prefix, // tag_type_id varuint
        schema->prefix, // fcount read
        schema->prefix, schema->prefix, schema->prefix, // fields allocation
        schema->prefix, // field name
        schema->prefix, // field id
        schema->prefix, // field type id
        schema->prefix, // bit width
        schema->prefix, // type ref in resolver
        schema->prefix, // TypeDescription
        schema->prefix, schema->prefix, // type_descriptions size uses
        schema->prefix, schema->prefix, // strcmp type_descriptions
        schema->prefix, // desc pointer
        schema->prefix); // SchemaField in mapping

    sb_append(impl,
        "const uint8_t *%sget_schema_blob(size_t *out_size) {\n"
        "    if (out_size) { *out_size = sizeof(%sschema_blob); }\n"
        "    return %sschema_blob;\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix);

    sb_append(impl,
        "const %sSchemaInfo *%sget_embedded_schema(%sBuffer *allocator) {\n"
        "    return %sparse_schema(%sschema_blob, sizeof(%sschema_blob), allocator);\n"
        "}\n\n",
        schema->prefix, schema->prefix, schema->prefix,
        schema->prefix, schema->prefix, schema->prefix);
    
    // Implement skip_generic
    sb_append(impl,
        "bool %sskip_generic(%sBuffer *buffer, uint32_t type_id, bool is_array, const %sSchemaInfo *schema) {\n"
        "    if (is_array) {\n"
        "        uint32_t count = 0;\n"
        "        if (!%sread_var_u32(buffer, &count)) return false;\n"
        "        for (uint32_t i = 0; i < count; ++i) {\n"
        "            if (!%sskip_generic(buffer, type_id, false, schema)) return false;\n"
        "        }\n"
        "        return true;\n"
        "    }\n"
        "    // Builtins\n"
        "    if (type_id < 11) {\n"
        "        switch (type_id) {\n"
        "            case 0: // u8\n"
        "            case 4: // i8\n"
        "            case 10: // bool\n"
        "                return %sbuffer_skip_bytes(buffer, 1);\n"
        "            case 1: // u16\n"
        "            case 2: // u32\n"
        "            case 5: // i16\n"
        "            case 6: // i32\n"
        "            {\n"
        "                uint32_t tmp; return %sread_var_u32(buffer, &tmp);\n"
        "            }\n"
        "            case 3: // u64\n"
        "            case 7: // i64\n"
        "            {\n"
        "                uint64_t tmp; return %sread_var_u64(buffer, &tmp);\n"
        "            }\n"
        "            case 8: // f32\n"
        "            {\n"
        "                float tmp; return %sread_compact_f32(buffer, &tmp);\n"
        "            }\n"
        "            case 9: // f64\n"
        "            {\n"
        "                double tmp; return %sread_compact_f64(buffer, &tmp);\n"
        "            }\n"
        "        }\n"
        "        return false;\n"
        "    }\n"
        "    // Look up in schema\n"
        "    if (!schema) return false;\n"
        "    const %sSchemaType *type = NULL;\n"
        "    for (uint32_t i = 0; i < schema->type_count; ++i) {\n"
        "        if (schema->types[i].type_id == type_id) {\n"
        "            type = &schema->types[i];\n"
        "            break;\n"
        "        }\n"
        "    }\n"
        "    if (!type) return false;\n"
        "    \n"
        "    if (type->kind == 0) { // ENUM\n"
        "        uint32_t tmp; return %sread_var_u32(buffer, &tmp);\n"
        "    }\n"
        "    \n"
        "    if (type->kind == 3) { // UNION\n"
        "        uint32_t tag = 0;\n"
        "        if (!%sread_var_u32(buffer, &tag)) return false;\n"
        "        const %sSchemaField *field = NULL;\n"
        "        for (uint32_t i = 0; i < type->field_count; ++i) {\n"
        "            if (type->fields[i].id == tag) { field = &type->fields[i]; break; }\n"
        "        }\n"
        "        if (!field) { return false; }\n"
        "        return %sskip_generic(buffer, field->type_id, field->is_array, schema);\n"
        "    }\n"
        "    \n"
        "    // STRUCT or MESSAGE\n"
        "    if (type->kind == 2) { // MESSAGE\n"
        "        // Messages are length-delimited; skip fields until the terminator tag.\n"
        "        while (true) {\n"
        "            uint32_t field_id = 0;\n"
        "            %swireType wire = %swireType_Varint;\n"
        "            if (!%sread_wire_tag(buffer, &field_id, &wire)) return false;\n"
        "            if (field_id == 0) break;\n"
        "            if (!%sskip_wire_value(buffer, wire)) return false;\n"
        "        }\n"
        "        return true;\n"
        "    }\n"
        "    \n"
        "    // STRUCT\n"
        "    // Read bitmask for optional fields\n"
        "    uint32_t optional_count = 0;\n"
        "    bool has_bitfields = false;\n"
        "    for (uint32_t i = 0; i < type->field_count; ++i) {\n"
        "        if (type->fields[i].is_removed) continue;\n"
        "        if (type->fields[i].is_optional) optional_count++;\n"
        "        if (type->fields[i].bit_width > 0) has_bitfields = true;\n"
        "    }\n"
        "    %sBitReader bit_reader = {0, 8};\n"
        "    uint8_t *bitmask = NULL;\n"
        "    if (optional_count > 0) {\n"
        "        uint32_t bytes = (optional_count + 7) / 8;\n"
        "        if (bytes > 128) return false;\n"
        "        uint8_t mask_buf[128];\n"
        "        if (!%sbuffer_read_bytes(buffer, mask_buf, bytes)) return false;\n"
        "        bitmask = mask_buf;\n"
        "    }\n"
        "    \n"
        "    uint32_t opt_idx = 0;\n"
        "    for (uint32_t i = 0; i < type->field_count; ++i) {\n"
        "        if (type->fields[i].is_removed) continue;\n"
        "        bool present = true;\n"
        "        if (type->fields[i].is_optional) {\n"
        "            present = (bitmask[opt_idx / 8] >> (opt_idx % 8)) & 1;\n"
        "            opt_idx++;\n"
        "        }\n"
        "        if (has_bitfields && type->fields[i].bit_width == 0) { %sbitreader_align(&bit_reader); }\n"
        "        if (present) {\n"
        "            if (type->fields[i].bit_width > 0) {\n"
        "                uint64_t tmp = 0;\n"
        "                if (!%sbitreader_read(&bit_reader, buffer, type->fields[i].bit_width, &tmp)) return false;\n"
        "            } else if (!%sskip_generic(buffer, type->fields[i].type_id, type->fields[i].is_array, schema)) return false;\n"
        "        }\n"
        "    }\n"
        "    return true;\n"
        "}\n\n",
        schema->prefix, // skip_generic name
        schema->prefix, // Buffer
        schema->prefix, // SchemaInfo
        schema->prefix, // read_var_u32 (array)
        schema->prefix, // recursive skip
        schema->prefix, // buffer_skip_bytes
        schema->prefix, // read_var_u32
        schema->prefix, // read_var_u64
        schema->prefix, // read_compact_f32
        schema->prefix, // read_compact_f64
        schema->prefix, // SchemaType
        schema->prefix, // enum read_var_u32
        schema->prefix, // union tag read_var_u32
        schema->prefix, // SchemaField
        schema->prefix, // recursive union skip
        schema->prefix, // wireType
        schema->prefix, // wireType
        schema->prefix, // read_wire_tag
        schema->prefix, // skip_wire_value
        schema->prefix, // BitReader
        schema->prefix, // buffer_read_bytes
        schema->prefix, // bitreader_align
        schema->prefix, // bitreader_read
        schema->prefix); // recursive struct skip
}

int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "usage: %s <schema.hischema> <output.h> [--ignore-compat]\n", argv[0]);
        return EXIT_FAILURE;
    }
    bool ignore_compat = false;
    for (int i = 3; i < argc; ++i) {
        if (strcmp(argv[i], "--ignore-compat") == 0) {
            ignore_compat = true;
        } else {
            fatal("schema_gen: unknown option '%s'", argv[i]);
        }
    }
    const char *schema_path = argv[1];
    const char *output_path = argv[2];
    size_t file_size = 0;
    char *file_data = read_entire_file(schema_path, &file_size);
    Schema schema;
    schema_init(&schema);
    schema.filename = schema_path;
    Parser parser;
    parser_init(&parser, file_data, schema_path);
    parse_schema(&parser, &schema);

    char bin_path[1024];
    size_t len = strlen(output_path);
    if (len > 2 && strcmp(output_path + len - 2, ".h") == 0) {
        snprintf(bin_path, sizeof(bin_path), "%.*s.hischemabin", (int)(len - 2), output_path);
    } else {
        snprintf(bin_path, sizeof(bin_path), "%s.hischemabin", output_path);
    }

    if (!ignore_compat && file_exists(bin_path)) {
        check_backward_compatibility(bin_path, &schema);
    }
    
    // Generate binary schema data
    StringBuilder bin_sb;
    sb_init(&bin_sb);
    generate_binary_schema_data(&schema, &bin_sb);

    // Write binary schema file
    FILE *f = fopen(bin_path, "wb");
    if (!f) { fatal("schema_gen: failed to open '%s' for writing", bin_path); }
    fwrite(bin_sb.data, 1, bin_sb.count, f);
    fclose(f);

    generate_header(&schema, schema_path, output_path, &bin_sb);
    
    free(bin_sb.data);
    free(file_data);
    return EXIT_SUCCESS;
}
