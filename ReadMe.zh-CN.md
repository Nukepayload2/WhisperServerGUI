# Whisper Server GUI
[Whisper 服务器](https://github.com/ggml-org/whisper.cpp/tree/master/examples/server)的桌面客户端

## 开发环境
- .NET SDK 10.0

## 运行环境
- Windows / Linux / macOS 桌面
- .NET Runtime 10.0
- 暂不支持 AOT 编译，如有需求请及时提出，并且暂时用自包含 R2R 编译

## 用法
- 下载 Whisper 服务器的编译产物
- 下载 Whisper 模型
- 启动本程序
- 配置 Whisper 服务器和模型
- 点击启动服务
- 切换到推理界面，拖放文件进行语音识别

## 功能
- 完整的启动命令行参数配置，包括高级功能
- 预览启动命令
- 记忆上次启动和关闭程序的配置
- 查看服务端的命令行输出
- 批量文件推理
- 并行请求推理
- 中途取消推理
- 完整的输出格式支持
- 将推理结果输出到文件
- 将输出合并为 CSV 文件
