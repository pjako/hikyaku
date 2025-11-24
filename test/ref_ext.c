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
    gen_Achievements achievements[3] = {
        {.questsCompleted = 5, .bossesDefeated = 1, .secretsFound = 3, .rank = 2, .prestigeLevel = 0},
        {.questsCompleted = 12, .bossesDefeated = 3, .secretsFound = 4, .rank = 4, .prestigeLevel = 1},
        {.questsCompleted = 30, .bossesDefeated = 9, .secretsFound = 7, .rank = 5, .prestigeLevel = 2},
    };
    gen_Player player = {0};
    player.has_id = true;
    player.id = 77;
    player.has_class = true;
    player.class = gen_Class_summoner;
    player.has_achievements = true;
    player.achievements.count = 3;
    player.achievements.items = achievements;

    uint8_t encoded[512];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_Player_encode_compact(&player, &w, schema));

    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[512];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));

    gen_Player decoded = {0};
    CHECK(gen_Player_decode_compact(&decoded, &r, &mem, schema));

    CHECK(decoded.id == 77);
    CHECK(decoded.class == gen_Class_summoner);
    CHECK(decoded.achievements.count == 3);
    CHECK(decoded.achievements.items[0].questsCompleted == 5);
    CHECK(decoded.achievements.items[2].prestigeLevel == 2);
    return 0;
}

static int test_entity_state_optionals(const gen_SchemaInfo *schema) {
    gen_PlayerState state = {0};
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

    gen_PlayerState decoded = {0};
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
    gen_AchievementsArray arr = {0};
    gen_Achievements achievements[2] = {
        {.questsCompleted = 1, .bossesDefeated = 0, .secretsFound = 2, .rank = 1, .prestigeLevel = 0},
        {.questsCompleted = 9, .bossesDefeated = 1, .secretsFound = 4, .rank = 2, .prestigeLevel = 0},
    };
    arr.count = 2;
    arr.items = achievements;

    uint8_t encoded[128];
    gen_Buffer w;
    init_buf(&w, encoded, sizeof(encoded));
    CHECK(gen_AchievementsArray_write(&arr, &w, schema));

    gen_AchievementsArray decoded = {0};
    gen_Buffer r;
    init_buf(&r, encoded, w.used);
    uint8_t arena[128];
    gen_Buffer mem;
    init_buf(&mem, arena, sizeof(arena));
    CHECK(gen_AchievementsArray_read(&decoded, &r, &mem, schema));
    CHECK(decoded.count == 2);
    CHECK(decoded.items[1].bossesDefeated == 1);
    return 0;
}

static int test_skip_generic(const gen_SchemaInfo *schema) {
    gen_Achievements achievements[1] = { {.questsCompleted = 2, .bossesDefeated = 1, .secretsFound = 1, .rank = 1, .prestigeLevel = 0} };
    gen_Player player = {0};
    player.has_id = true;
    player.id = 123;
    player.has_achievements = true;
    player.achievements.count = 1;
    player.achievements.items = achievements;

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
