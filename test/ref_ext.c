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
    gen_Achievements colors[3] = {
        {.red = 10, .green = 0xAA, .blue = 0x55, .alpha = 0xFF},
        {.red = 20, .green = 30, .blue = 40, .alpha = 50},
        {.red = 200, .green = 150, .blue = 100, .alpha = 50},
    };
    gen_Player player;
    gen_Player_defaults(&player);
    player.has_id = true;
    player.id = 77;
    player.has_class = true;
    player.class = gen_Class_summoner;
    player.has_colors = true;
    player.colors.count = 3;
    player.colors.items = colors;

    uint8_t encoded[512];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_Player_encode_compact(&player, &w, schema));

    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[512];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));

    gen_Player decoded;
    gen_Player_defaults(&decoded);
    CHECK(gen_Player_decode_compact(&decoded, &r, &mem, schema));

    CHECK(decoded.id == 77);
    CHECK(decoded.class == gen_Class_summoner);
    CHECK(decoded.colors.count == 3);
    CHECK(decoded.colors.items[0].alpha == 0xFF);
    CHECK(decoded.colors.items[2].green == 150);
    return 0;
}

static int test_entity_state_optionals(const gen_SchemaInfo *schema) {
    gen_PlayerState state;
    gen_PlayerState_defaults(&state);
    state.has_yaw = true;
    state.yaw = 17;
    state.has_posX = true;
    state.posX = -1234;
    state.has_posY = false;
    state.has_posZ = true;
    state.posZ = 42;

    uint8_t encoded[128];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_PlayerState_encode_compact(&state, &w, schema));

    gen_PlayerState decoded;
    gen_PlayerState_defaults(&decoded);
    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[128];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));

    CHECK(gen_PlayerState_decode_compact(&decoded, &r, &mem, schema));
    CHECK(decoded.has_yaw && decoded.yaw == 17);
    CHECK(decoded.has_posX && decoded.posX == -1234);
    CHECK(!decoded.has_posY);
    CHECK(decoded.has_posZ && decoded.posZ == 42);
    return 0;
}

static int test_array_read_write(const gen_SchemaInfo *schema) {
    (void)schema;
    gen_ArrayAchievements arr = {0};
    gen_Achievements colors[2] = {
        {.red = 1, .green = 2, .blue = 3, .alpha = 4},
        {.red = 5, .green = 6, .blue = 7, .alpha = 8},
    };
    arr.count = 2;
    arr.items = colors;

    uint8_t encoded[128];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_ArrayAchievements_write(&arr, &w, schema));

    gen_ArrayAchievements decoded = {0};
    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[128];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));
    CHECK(gen_ArrayAchievements_read(&decoded, &r, &mem, schema));
    CHECK(decoded.count == 2);
    CHECK(decoded.items[1].blue == 7);
    return 0;
}

static int test_skip_generic(const gen_SchemaInfo *schema) {
    gen_Achievements colors[1] = { {.red = 9, .green = 9, .blue = 9, .alpha = 9} };
    gen_Player player;
    gen_Player_defaults(&player);
    player.has_id = true;
    player.id = 123;
    player.has_colors = true;
    player.colors.count = 1;
    player.colors.items = colors;

    uint8_t encoded[256];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_Player_encode_compact(&player, &w, schema));

    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    CHECK(gen_skip_generic(&r, gen_type_Player, false, schema));
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
