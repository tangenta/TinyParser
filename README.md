# homework-TinyParser

文件结构：
```
.
├── build.sbt
├── LICENSE
├── README.md
├── src
│   ├── main
│   │   ├── java
│   │   │   ├── MainWindow.form  # 用户界面
│   │   │   └── MainWindow.java
│   │   └── scala
│   │       ├── grammar
│   │       │   ├── Algorithm.scala  # 通用算法
│   │       │   └── Basic.scala    # 通用数据结构
│   │       └── tiny
│   │           ├── Parser.scala   # tiny解释器
│   │           ├── Productions.scala  # tiny文法规则
│   │           └── Scanner.scala  # tiny扫描分词器
│   └── test   # 测试
│       └── scala
│           ├── grammar
│           │   ├── AlgorithmTest.scala
│           │   ├── BasicSpec.scala
│           │   └── ParsingTableTest.scala
│           └── tiny
│               └── ScannerTest.scala
└── TinyParser-Introduction.pdf  # 项目介绍

```