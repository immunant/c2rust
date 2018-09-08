
pub const DEFAULT_CONFIG: &str = r###"
---
- file: "*.rs"
  priority: -1000000
  items:
        # Matches the clang-plugin behavior for this structure
        - item: struct
          name: "sockaddr_storage"
          fields:
              __ss_padding: disabled

        # Matches the clang-plugin behavior for this structure
        - item: struct
          name: "_IO_FILE"
          custom_hash: "0x72617453454c4946_u64"
          custom_hash_format: "expression"

        # Matches the clang-plugin behavior for this structure
        - item: struct
          name: "_IO_FILE_complete"
          custom_hash: "0x72617453454c4946_u64"
          custom_hash_format: "expression"

...
"###;
