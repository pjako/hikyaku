#include <stdio.h>
#include <stdint.h>
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef uint64_t usize;

typedef uintptr_t umm;
typedef size_t mms;

typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef int64_t  isize;

typedef float    f32;
typedef double   f64;

#define sizeOf(S) ((i64)sizeof(S))
#define countOf(V) (sizeOf(V) / sizeOf((V)[0]))


#define GEN__IMPLEMENTATION
#include "ref.h"

int main() {
    gen_Color colors[2] = {
        {.red = 33, .alpha = 22},
        {.red = 1, .alpha = 4}
    };
    gen_Example example = {
        .id = 22,
        .has_id = true,
        .has_type = false,
        .has_colors = true,
        .colors = {
            .count = countOf(colors),
            .items = &colors[0]
        },
    };

    uint8_t schemaStorage[1024*1024];
    gen_Buffer schemaBuffer;
    gen_buffer_init(&schemaBuffer, schemaStorage, countOf(schemaStorage));

    const gen_SchemaInfo* schemaInfo = gen_get_embedded_schema(&schemaBuffer);

    uint8_t storage[256];
    gen_Buffer write_buffer;
    gen_buffer_init(&write_buffer, storage, countOf(storage));
    if (!gen_Example_encode_compact(&example, &write_buffer, schemaInfo)) {
        fputs("encoding failed\n", stderr);
        return 1;
    }

    gen_Buffer read_buffer;
    gen_buffer_init(&read_buffer, storage, write_buffer.used);
    uint8_t memory_storage[256];
    gen_Buffer memory_buffer;
    gen_buffer_init(&memory_buffer, memory_storage, countOf(memory_storage));
    gen_Example decoded;
    gen_Example_defaults(&decoded);
    if (!gen_Example_decode_compact(&decoded, &read_buffer, &memory_buffer, schemaInfo)) {
        fputs("decoding failed\n", stderr);
        return 1;
    }

    printf("Decoded Example -> id=%u type=%d colors=%u\n",
           (unsigned)decoded.id, (int)decoded.type, (unsigned)decoded.colors.count);
    for (uint32_t i = 0; i < decoded.colors.count; ++i) {
        const gen_Color *c = &decoded.colors.items[i];
        printf("  Color %u: r=%u g=%u b=%u a=%u\n",
               (unsigned)i, (unsigned)c->red, (unsigned)c->green, (unsigned)c->blue, (unsigned)c->alpha);
    }

    return 0;
}
