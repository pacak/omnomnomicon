```
cargo run --features="rustyline" --example rustyline_ext
```


```
HFUZZ_BUILD_ARGS='--features=honggfuzz' cargo hfuzz run fuzz
```

```
HFUZZ_BUILD_ARGS='--features=honggfuzz'  HFUZZ_RUN_ARGS="--linux_perf_ipt_block --linux_perf_instr --linux_perf_branch" cargo hfuzz run-no-instr fuzz
```

```
HFUZZ_BUILD_ARGS='--features=honggfuzz' cargo hfuzz run-debug fuzz hfuzz_workspace/*/*.fuzz
```
