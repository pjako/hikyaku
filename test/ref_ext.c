#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef float    f32;
typedef double   f64;

#define GEN__IMPLEMENTATION
#include "ref.h"

#define CHECK(x) do { if (!(x)) { fprintf(stderr, "CHECK failed: %s at %s:%d\n", #x, __FILE__, __LINE__); return 1; } } while (0)

static void init_buf(gen_Buffer *buf, void *storage, size_t size) {
    gen_buffer_init(buf, storage, (uint32_t)size);
}

static int test_example_roundtrip(const gen_SchemaInfo *schema) {
    gen_Color colors[3] = {
        {.red = 10, .green = 0xAA, .blue = 0x55, .alpha = 0xFF},
        {.red = 20, .green = 30, .blue = 40, .alpha = 50},
        {.red = 200, .green = 150, .blue = 100, .alpha = 50},
    };
    gen_Example example = {
        .id = 77,
        .has_id = true,
        .type = gen_shapeType_pointed,
        .colors.count = 3,
        .colors.items = colors,
        .has_colors = true,
    };

    uint8_t encoded[512];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_Example_encode_compact(&example, &w, schema));

    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[512];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));

    gen_Example decoded;
    gen_Example_defaults(&decoded);
    CHECK(gen_Example_decode_compact(&decoded, &r, &mem, schema));

    CHECK(decoded.id == 77);
    CHECK(decoded.colors.count == 3);
    CHECK(decoded.colors.items[0].alpha == 0xFF);
    CHECK(decoded.colors.items[2].green == 150);
    return 0;
}

static int test_entity_state_optionals(const gen_SchemaInfo *schema) {
    gen_EntityState state;
    gen_EntityState_defaults(&state);
    state.id = 9001;
    state.yaw = 17;
    state.has_posX = true;
    state.posX = -1234;
    state.has_posY = false;

    uint8_t encoded[128];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_EntityState_encode_compact(&state, &w, schema));

    gen_EntityState decoded;
    gen_EntityState_defaults(&decoded);
    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[128];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));

    CHECK(gen_EntityState_decode_compact(&decoded, &r, &mem, schema));
    CHECK(decoded.id == 9001);
    CHECK(decoded.yaw == 17);
    CHECK(decoded.has_posX && decoded.posX == -1234);
    CHECK(!decoded.has_posY);
    return 0;
}

static int test_array_read_write(const gen_SchemaInfo *schema) {
    (void)schema;
    gen_ArrayColor arr = {0};
    gen_Color colors[2] = {
        {.red = 1, .green = 2, .blue = 3, .alpha = 4},
        {.red = 5, .green = 6, .blue = 7, .alpha = 8},
    };
    arr.count = 2;
    arr.items = colors;

    uint8_t encoded[128];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_ArrayColor_write(&arr, &w, schema));

    gen_ArrayColor decoded = {0};
    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[128];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));
    CHECK(gen_ArrayColor_read(&decoded, &r, &mem, schema));
    CHECK(decoded.count == 2);
    CHECK(decoded.items[1].blue == 7);
    return 0;
}

static int test_skip_generic(const gen_SchemaInfo *schema) {
    gen_Color colors[1] = { {.red = 9, .green = 9, .blue = 9, .alpha = 9} };
    gen_Example example = {
        .id = 123,
        .has_id = true,
        .type = gen_shapeType_round,
        .colors.count = 1,
        .colors.items = colors,
        .has_colors = true,
    };

    uint8_t encoded[256];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_Example_encode_compact(&example, &w, schema));

    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    CHECK(gen_skip_generic(&r, gen_type_Example, false, schema));
    CHECK(r.used == w.used);
    return 0;
}

int main(void) {
    uint8_t schema_storage[1024];
    gen_Buffer schema_buf;
    init_buf(&schema_buf, schema_storage, sizeof(schema_storage));
    const gen_SchemaInfo *schema = gen_get_embedded_schema(&schema_buf);
    CHECK(schema != NULL);

    CHECK(test_example_roundtrip(schema) == 0);
    CHECK(test_entity_state_optionals(schema) == 0);
    CHECK(test_array_read_write(schema) == 0);
    CHECK(test_skip_generic(schema) == 0);

    puts("extended tests passed");
    return 0;
}
