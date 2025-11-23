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
    gen_Achievements colors[2] = {
        {.red = 33, .green = 44, .blue = 55, .alpha = 22},
        {.red = 1, .green = 2, .blue = 3, .alpha = 4}
    };
    gen_Player player;
    gen_Player_defaults(&player);
    player.has_id = true;
    player.id = 22;
    player.has_class = true;
    player.class = gen_Class_fighter;
    player.has_colors = true;
    player.colors.count = (uint32_t)countOf(colors);
    player.colors.items = colors;

    uint8_t schemaStorage[1024*1024];
    gen_Buffer schemaBuffer;
    gen_buffer_init(&schemaBuffer, schemaStorage, countOf(schemaStorage));

    const gen_SchemaInfo* schemaInfo = gen_get_embedded_schema(&schemaBuffer);

    uint8_t storage[256];
    gen_Buffer write_buffer;
    gen_buffer_init(&write_buffer, storage, countOf(storage));
    if (!gen_Player_encode_compact(&player, &write_buffer, schemaInfo)) {
        fputs("encoding failed\n", stderr);
        return 1;
    }

    gen_Buffer read_buffer;
    gen_buffer_init(&read_buffer, storage, write_buffer.used);
    uint8_t memory_storage[256];
    gen_Buffer memory_buffer;
    gen_buffer_init(&memory_buffer, memory_storage, countOf(memory_storage));
    gen_Player decoded;
    gen_Player_defaults(&decoded);
    if (!gen_Player_decode_compact(&decoded, &read_buffer, &memory_buffer, schemaInfo)) {
        fputs("decoding failed\n", stderr);
        return 1;
    }

    printf("Decoded Player -> id=%u class=%d colors=%u\n",
           (unsigned)decoded.id, (int)decoded.class, (unsigned)decoded.colors.count);
    for (uint32_t i = 0; i < decoded.colors.count; ++i) {
        const gen_Achievements *c = &decoded.colors.items[i];
        printf("  Achievement %u: r=%u g=%u b=%u a=%u\n",
               (unsigned)i, (unsigned)c->red, (unsigned)c->green, (unsigned)c->blue, (unsigned)c->alpha);
    }

    return 0;
}
