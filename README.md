### Prepare

```
opam pin add mirage-nat https://github.com/yomimono/mirage-nat.git
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
