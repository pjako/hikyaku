# hikyaku

`hikyaku`—named after the historic Japanese couriers—is a single-file, public-domain schema compiler that converts a small `.cm` schema language into a C99 header containing struct definitions, buffer helpers, and serialization code. It is tailored for compact network messages where allocating a heavyweight runtime (e.g. Protocol Buffers or FlatBuffers) would be overkill.

## Highlights
- Generates header-only C APIs and matching binary schema blobs from a single schema file.
- Produces both struct/message definitions and encode/decode/skip helpers for **Standard** (wire-tag) and **Compact** (tagless) encodings.
- Works entirely on `stdint.h`-style primitives plus enums/structs, no external dependencies beyond the C standard library.
- Handles optional fields, arrays, user-defined types/aliases, endianness, and schema reflection data for runtime validation.

## Building
```bash
cc -std=c99 -O2 schema_gen.c -o schema_gen
```
The program is intentionally freestanding—no libraries beyond the C runtime are required. Any C99-capable compiler should work (clang, GCC, MSVC with minimal adjustments, etc.).

## Usage
```bash
./schema_gen input.cm output.h [--ignore-compat]
```

This emits:
1. `output.h` – the generated header (definitions + optional implementation when `PREFIX_IMPLEMENTATION` is defined).
2. `output.hibinschema` – a compact binary schema that mirrors the textual schema. It can be parsed at runtime via `parse_schema` or embedded through helper APIs.

By default the generator will **refuse to overwrite** an existing `output.hibinschema` if it detects backwards-incompatible changes (e.g., enum/struct removal, changing field types/flags/ids). Pass `--ignore-compat` to skip this guard when you intentionally break compatibility.

### Compatibility guard (why changes are blocked)
Binary schemas are meant to be stable contracts: downstream code that reads an older stream must be able to decode packets emitted by newer writers. Because of that the generator enforces:
- Existing enums/structs/messages cannot be removed or have their kind changed.
- Fields in structs cannot be renamed, retyped, reordered, or have modifiers (array/optional/deprecated) changed, and structs cannot grow.
- Message fields cannot be renamed/retyped/reordered; ids are stable and blocked once used (or marked `[removed]`). Messages may add new fields with fresh ids.

If you truly need a breaking change, bump to a new type/message name or supply `--ignore-compat` for that run and accept the ABI break.

Define `PREFIX_IMPLEMENTATION` (derived from your schema prefix, see below) in exactly one translation unit before including the header to generate the function bodies.

## Schema Language
The `.cm` language is kept deliberately small:

| Construct | Description |
|-----------|-------------|
| `prefix foo_` | Sets the C symbol prefix (`foo_` by default). |
| `type logical u32` | Adds/replaces a type alias that maps schema names to C types. |
| `enum Color { red, green, blue=5 }` | Declares enums (optional base type via `enum Kind : u16`). |
| `bitset Flags : u16 { a, b=4, c }` | Declares a flag set with single-bit values on an unsigned base (u8/u16/u32/u64). |
| `struct Packet { ... }` | Declares POD structs encoded compactly. |
| `message Ping { ... }` | Similar to `struct`, but when encoded in Standard mode each field is tagged (`id = N` fields reserve IDs). |

Within structs/messages:
- Arrays: `u8[] payload;`
- Optional fields: `u32 checksum?;` (encoded via leading bitmask).
- Bit-width hints: `u16 flags : 4;` (declares a C bitfield; in compact encoding these bit-width scalars are packed LSB-first and padded to the next byte before the next non-bit-width field; Standard encoding still masks then writes the full scalar varint).
- Message field ids: `u32 sequence = 1;` assigns a stable numeric id. Removed ids stay blocked via `[removed]`.

Message field numbering (Standard encoding):
- Every message field must declare an explicit numeric id (`= N`).
- Ids are immutable once published; changing or reusing an id is an error.
- Retired ids stay blocked forever via `[removed]`; unknown/removed ids are skipped safely while decoding.

Messages automatically synthesize an `id` field when `id = <number>;` appears inside the definition.

Bit-width annotations now affect both the generated C struct layout and compact wire format: consecutive bit-width scalars are appended bit-by-bit (LSB-first), then padded to the next byte boundary before any following non-bit-width field or at struct end. Optional fields still use the leading presence bitmask, and arrays cannot specify bit widths.

## Example Schema (feature tour)
Below is a compact `.cm` file that exercises the language surface—prefixing, aliases, enums with explicit bases, structs vs. messages, optionals, arrays, and bitfields:

```cm
// example.cm
prefix courier_

// Remap schema names to C types used in your codebase
type handle u32
type coord i32

enum Status : u8 {
  ok = 0,
  busy = 3,
  gone = 9
}

struct Attachment {
  u32 size_bytes;
  u8 bytes[];
}

struct Parcel {
  handle id;
  Status state;
  coord x;
  coord y;
  coord z? : 20;      // optional C bitfield; compact encoding packs 20 bits and pads before the next field
  u16 flags : 4;      // bitfield in the struct layout; packed alongside other bit-width fields in compact mode
  u8 tags[];          // arrays get a count + items
  Attachment extra?;  // optional nested struct
  u32 checksum?;
}

bitset Access : u8 {
  read,               // 1 << 0
  write,              // 1 << 1
  execute = 1 << 3    // custom bit value is allowed but must be a single bit
}

message Envelope {
  u32 sequence = 1;   // explicit field ids for messages (stable once published)
  Parcel body = 2;    // structs nested inside messages use Compact encoding
  u8 signature[] = 3;
}
```

Generate the corresponding C99 API and binary schema with `./schema_gen example.cm example.h`; the emitted symbols will be prefixed with `courier_`.

## Generated Header Layout
The header stitches several pieces together (guarded by feature macros derived from your prefix, e.g. `GEN_`):

1. **Buffer utilities**: `Buffer` plus aligned push/pop helpers to treat caller-provided memory as arenas.
2. **Wire helpers**: Varint encoders, ZigZag helpers, compact floating-point encoding, and Protobuf-like wire tag readers/writers.
3. **Type declarations**: Forward declarations followed by enums, arrays, and structs/messages.
4. **I/O helpers**: `read`/`write` routines for every array/struct plus `*_encode_compact`, `*_decode_compact`, and `*_skip_compact`.
5. **Metadata**: Generated `TypeDescription` and `ParameterInfo` tables (exposed via `get_type_description`) that make offsets and field IDs available at runtime.
6. **Runtime schema**: Embedded binary schema blob, parsing helpers (`parse_schema`, `get_embedded_schema`), and `skip_generic` for schema-driven skipping when introspecting encoded streams.

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
- Bit-width scalars are bit-packed LSB-first; padding is inserted to the next byte before any non-bit-width field.
- Arrays encode a count followed by the items (recursively encoded).
- Perfect for deterministic packet layouts or bandwidth-sensitive transports.

The generated `*_encode_compact`, `*_decode_compact`, and `*_skip_compact` helpers implement this format, while message codecs produce/consume the Standard wire format.

## Runtime Schema & Reflection
To introspect schemas at runtime (e.g. for tooling, validation, or “slow path” decoding against arbitrary blobs):

```c
Buffer arena;
uint8_t storage[4096];
buffer_init(&arena, storage, sizeof(storage));

const SchemaInfo *info = get_embedded_schema(&arena);
```

`SchemaInfo` exposes every type, its fields, and flags (array/optional). `skip_generic` can use those descriptions to skip unfamiliar fields safely when a runtime schema accompanies the data stream.

## License
Released under the Unlicense (public domain dedication). See `LICENSE` or the header comment in `schema_gen.c` for details. Attribution is appreciated but not required.
