### Prepare

```
opam pin add arbitrary-graph https://github.com/yomimono/arbitrary-network.git
opam pin add mirage-nat https://github.com/yomimono/ocaml-nat.git
```

(optional)
```
opam pin add lwt 'https://github.com/mirage/lwt.git#tracing
```

### Compile

```
mirage configure
make
```