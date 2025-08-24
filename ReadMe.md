# Whisper Server GUI

Desktop client for [Whisper server](https://github.com/ggml-org/whisper.cpp/tree/master/examples/server)

[中文说明](ReadMe.zh-CN.md)

## Development Environment
- .NET SDK 9.0

## Runtime Environment
- Windows / Linux / macOS desktop
- .NET Runtime 9.0
- AOT compilation is not currently supported. Please raise an issue if needed, and use self-contained R2R compilation for now.

## Usage
- Download the compiled artifacts of the Whisper server
- Download the Whisper model
- Start this program
- Configure the Whisper server and model
- Click "Start Service"
- Switch to the inference interface and drag and drop files for speech recognition

## Features
- Complete startup command-line parameter configuration, including advanced features
- Preview startup command
- Remember configuration from last startup and shutdown
- View server command-line output
- Batch file inference
- Parallel request inference
- Cancel inference mid-process
- Full output format support
- Output inference results to files
- Merge outputs into CSV files
