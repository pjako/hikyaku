# schema_gen

`schema_gen` is a single-file, public-domain schema compiler that converts a small `.cm` schema language into a C99 header containing struct definitions, buffer helpers, and serialization code. It is tailored for compact network messages where allocating a heavyweight runtime (e.g. Protocol Buffers or FlatBuffers) would be overkill.

## Highlights
- Generates header-only C APIs and matching binary schema blobs from a single schema file.
- Produces both struct/message definitions and encode/decode/skip helpers for **Standard** (wire-tag) and **Compact** (tagless) encodings.
- Works entirely on `stdint.h`-style primitives plus enums/structs, no external dependencies beyond the C standard library.
- Handles optional fields, arrays, user-defined types/aliases, defaults, endianness, and schema reflection data for runtime validation.

## Building
```bash
cc -std=c99 -O2 schema_gen.c -o schema_gen
```
The program is intentionally freestanding—no libraries beyond the C runtime are required. Any C99-capable compiler should work (clang, GCC, MSVC with minimal adjustments, etc.).

## Usage
```bash
./schema_gen input.cm output.h
```

This emits:
1. `output.h` – the generated header (definitions + optional implementation when `SCHEMA_PREFIX_IMPLEMENTATION` is defined).
2. `output.bschema` – a compact binary schema that mirrors the textual schema. It can be parsed at runtime via `prefixparse_schema` or embedded through helper APIs.

Define `PREFIX_IMPLEMENTATION` (derived from your schema prefix, see below) in exactly one translation unit before including the header to generate the function bodies.

## Schema Language
The `.cm` language is kept deliberately small:

| Construct | Description |
|-----------|-------------|
| `prefix foo_` | Sets the C symbol prefix (`foo_` by default). |
| `type logical u32` | Adds/replaces a type alias that maps schema names to C types. |
| `enum Color { red, green, blue=5 }` | Declares enums (optional base type via `enum Kind : u16`). |
| `struct Packet { ... }` | Declares POD structs encoded compactly. |
| `message Ping { ... }` | Similar to `struct`, but when encoded in Standard mode each field is tagged (`id = N` fields reserve IDs). |

Within structs/messages:
- Arrays: `u8[] payload;`
- Optional fields: `u32 checksum?;` (encoded via leading bitmask).
- Bitfields: `u16 flags : 4;` (read/write as scalars, stored in bitfield slots when emitted).
- Defaults: `u8 version = 1;` or enum defaults `Mode mode = ACTIVE;`.

Messages automatically synthesize an `id` field when `id = <number>;` appears inside the definition.

## Generated Header Layout
The header stitches several pieces together (guarded by feature macros derived from your prefix, e.g. `GEN_`):

1. **Buffer utilities**: `prefixBuffer` plus aligned push/pop helpers to treat caller-provided memory as arenas.
2. **Wire helpers**: Varint encoders, ZigZag helpers, compact floating-point encoding, and Protobuf-like wire tag readers/writers.
3. **Type declarations**: Forward declarations followed by enums, arrays, structs, and their default initializers (`prefixFoo_defaults`).
4. **I/O helpers**: `read`/`write` routines for every array/struct plus `*_encode_compact`, `*_decode_compact`, and `*_skip_compact`.
5. **Metadata**: Generated `TypeDescription` and `ParameterInfo` tables (exposed via `prefixget_type_description`) that make offsets and field IDs available at runtime.
6. **Runtime schema**: Embedded binary schema blob, parsing helpers (`prefixparse_schema`, `prefixget_embedded_schema`), and `prefixskip_generic` for schema-driven skipping when introspecting encoded streams.

Including the header with `PREFIX_IMPLEMENTATION` defined appends the implementation block right after the declarations, keeping integration simple.

## Encoding Modes
The top of `schema_gen.c` documents both encodings. In practice:

### Standard (Tag-Length-Value)
- Field wire tags mirror protobuf, but the tag layout uses 2 bits for wire type.
- Messages encode each present field as `[tag][value]...` followed by a zero tag terminator.
- Structs nested inside Standard messages are length-delimited blocks encoded with their compact codec.

### Compact (Schema-Driven)
- Structs are emitted field-by-field without tags.
- Optional fields share a leading bitmask (`ceil(optional_fields / 8)` bytes).
- Scalars use varints, ZigZag, or compact floats (`flag byte + payload`) depending on type.
- Arrays encode a count followed by the items (recursively encoded).
- Perfect for deterministic packet layouts or bandwidth-sensitive transports.

The generated `*_encode_compact`, `*_decode_compact`, and `*_skip_compact` helpers implement this format, while message codecs produce/consume the Standard wire format.

## Runtime Schema & Reflection
To introspect schemas at runtime (e.g. for tooling, validation, or “slow path” decoding against arbitrary blobs):

```c
prefixBuffer arena;
uint8_t storage[4096];
prefixbuffer_init(&arena, storage, sizeof(storage));

const prefixSchemaInfo *info = prefixget_embedded_schema(&arena);
```

`prefixSchemaInfo` exposes every type, its fields, and flags (array/optional). `prefixskip_generic` can use those descriptions to skip unfamiliar fields safely when a runtime schema accompanies the data stream.

## Tips
- Always zero and initialize structs via the generated `prefixFoo_defaults` before mutating.
- When decoding arrays/structs, pass a scratch `prefixBuffer` arena to hold nested allocations.
- To extend built-in aliases, use repeated `type` statements; redefining a type updates the mapping.
- The tool emits human-readable errors (`schema_gen: line ...`) for malformed schemas.

## License
Public domain. Where that dedication is not recognized, a perpetual, irrevocable license is granted to copy, distribute, and modify the code. (See the license text embedded at the top of `schema_gen.c`.)
