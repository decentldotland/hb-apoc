# HyperBEAM Function Reference Data

This directory contains structured JSON data files parsed from the HyperBEAM function reference documentation.

## Data Structure

The function reference data is organized in a hierarchical structure:

```
functions/
├── index.json                # Global index of all subsystems
├── core.json                 # Core subsystem index
├── storage.json              # Storage subsystem index
├── network.json              # Network subsystem index
├── codec.json                # Codec subsystem index
├── arweave.json              # Arweave subsystem index
├── app.json                  # Application Management subsystem index
├── devices.json              # Device Ecosystem subsystem index
├── core/                     # Core subsystem modules
│   ├── hb_converge.json      # Convergence Protocol module functions
│   ├── hb_message.json       # Message Handling module functions
│   ├── hb_path.json          # Path Routing module functions
│   ├── hb_core.json          # Core Application module functions
│   ├── hb_opts.json          # Configuration module functions
│   └── hb_util.json          # Utility Functions module functions
├── storage/                  # Storage subsystem modules
│   ├── ...
├── network/                  # Network subsystem modules
│   ├── ...
├── codec/                    # Codec subsystem modules
│   ├── ...
├── arweave/                  # Arweave subsystem modules
│   ├── ...
├── app/                      # Application Management subsystem modules
│   ├── ...
└── devices/                  # Device Ecosystem subsystem modules
    ├── ...
```

## Data Formats

### Global Index (`index.json`)

```json
{
  "subsystems": [
    {
      "id": "core",
      "name": "Core Infrastructure",
      "count": 124,
      "modules": [
        {
          "id": "hb_converge",
          "name": "Convergence Protocol",
          "count": 27
        },
        // More modules...
      ]
    },
    // More subsystems...
  ]
}
```

### Subsystem Index (e.g., `core.json`)

```json
{
  "name": "Core Infrastructure",
  "description": "Core functions that provide the foundation of the HyperBEAM system...",
  "modules": [
    {
      "id": "hb_converge",
      "name": "Convergence Protocol",
      "count": 27
    },
    // More modules...
  ]
}
```

### Module Functions (e.g., `core/hb_converge.json`)

```json
[
  {
    "name": "hb_converge:process/2",
    "module": "hb_converge",
    "moduleId": "hb_converge",
    "functionName": "process",
    "arity": 2,
    "description": "Processes a message according to the convergence protocol.",
    "apiType": "public",
    "sourceFile": "src/hb_converge.erl",
    "lineNumber": 124,
    "params": [
      {
        "name": "Message",
        "type": "map()",
        "description": "The message to process."
      },
      // More parameters...
    ],
    "returns": [
      {
        "type": "{ok, NewState}",
        "description": "The updated state after processing the message."
      },
      // More return values...
    ],
    "examples": [
      "Example code here..."
    ]
  },
  // More functions...
]
```

## Integration

The JSON data files in this directory are imported by the function reference pages to display the documentation. The parser script (`scripts/parse_function_reference.js`) extracts the data from the Markdown documentation and generates these JSON files.

To update the function data:

1. Run the parser script:
   ```
   node scripts/parse_function_reference.js
   ```

2. The script will read the function reference Markdown file and generate the JSON files.

3. The React components will automatically load the updated data when the site is built.

## Modularity Benefits

This structure provides several benefits:

1. **Separation of Data and UI**: The function data is stored separately from the UI components, making it easier to maintain and update.

2. **Granular Loading**: Each page only loads the data it needs, making the site more efficient.

3. **Better Performance**: Breaking down the function data into smaller files helps reduce initial load times and improves page performance.

4. **Easier Maintenance**: When the function documentation changes, only the affected files need to be updated.
