#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define GEN__IMPLEMENTATION
#include "map_test.h"

static int check(bool ok, const char *msg) {
    if (!ok) {
        fprintf(stderr, "map_test: %s\n", msg);
        return 1;
    }
    return 0;
}

int main(void) {
    gen_Player player = {0};
    player.id = 1234;

    gen_StringAchievementPair pairs[2] = {
        {.key = {.content = (uint8_t *)"starter", .size = 7}, .value = {.achievementId = 7}},
        {.key = {.content = (uint8_t *)"boss", .size = 4}, .value = {.achievementId = 99}},
    };
    player.achievements.items = pairs;
    player.achievements.count = (uint32_t)(sizeof(pairs) / sizeof(pairs[0]));

    uint8_t schema_storage[1024];
    gen_Buffer schema_buf;
    gen_buffer_init(&schema_buf, schema_storage, (uint32_t)sizeof(schema_storage));
    const gen_SchemaInfo *schema = gen_get_embedded_schema(&schema_buf);

    uint8_t storage[256];
    gen_Buffer write_buf;
    gen_buffer_init(&write_buf, storage, (uint32_t)sizeof(storage));
    if (!gen_Player_encode_compact(&player, &write_buf, schema)) {
        fprintf(stderr, "encode failed\n");
        return 1;
    }

    gen_Player decoded = {0};
    gen_Buffer read_buf;
    gen_buffer_init(&read_buf, storage, write_buf.used);
    uint8_t arena_storage[256];
    gen_Buffer arena;
    gen_buffer_init(&arena, arena_storage, (uint32_t)sizeof(arena_storage));
    if (!gen_Player_decode_compact(&decoded, &read_buf, &arena, schema)) {
        fprintf(stderr, "decode failed\n");
        return 1;
    }

    if (check(decoded.achievements.count == player.achievements.count, "achievement count mismatch")) { return 1; }
    if (check(decoded.id == player.id, "id mismatch")) { return 1; }

    const char *expected_keys[] = {"starter", "boss"};
    const uint32_t expected_ids[] = {7u, 99u};
    for (uint32_t i = 0; i < decoded.achievements.count; ++i) {
        if (check(strcmp((const char *)decoded.achievements.items[i].key.content, expected_keys[i]) == 0, "key mismatch")) { return 1; }
        if (check(decoded.achievements.items[i].value.achievementId == expected_ids[i], "value mismatch")) { return 1; }
    }

    puts("map tests passed");
    return 0;
}
