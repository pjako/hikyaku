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
    gen_Achievements achievements[2] = {
        {.questsCompleted = 12, .bossesDefeated = 2, .secretsFound = 5, .rank = 3, .prestigeLevel = 1},
        {.questsCompleted = 34, .bossesDefeated = 5, .secretsFound = 9, .rank = 4, .prestigeLevel = 0}
    };
    gen_Player player = {0};
    player.idExist = true;
    player.id = 22;
    player.classExist = true;
    player.class = gen_Class_fighter;
    player.achievementsExist = true;
    player.achievements.count = (uint32_t)countOf(achievements);
    player.achievements.items = achievements;

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
    gen_Player decoded = {0};
    if (!gen_Player_decode_compact(&decoded, &read_buffer, &memory_buffer, schemaInfo)) {
        fputs("decoding failed\n", stderr);
        return 1;
    }

    printf("Decoded Player -> id=%u class=%d achievements=%u\n",
           (unsigned)decoded.id, (int)decoded.class, (unsigned)decoded.achievements.count);
    for (uint32_t i = 0; i < decoded.achievements.count; ++i) {
        const gen_Achievements *a = &decoded.achievements.items[i];
        printf("  Achievement %u: quests=%u bosses=%u secrets=%u rank=%u prestige=%u\n",
               (unsigned)i, (unsigned)a->questsCompleted, (unsigned)a->bossesDefeated,
               (unsigned)a->secretsFound, (unsigned)a->rank, (unsigned)a->prestigeLevel);
    }

    return 0;
}
