# Build with Jekyll

## Requirements
Installation of Jekyll is optional, as the building and serving can also performed with Docker. In this case, of course, Docker must be installed

## Build

### Build with Jekyll
Run:
```
jekyll build
```

### Build Docker
Run:

```
./build.sh
```

### Getting the resulting HTML pages
The resulting HTML pages will be stored in `_site_`, which is an ignored directory.

Just push the repo to see the results online.

## Serve

### Serve with Jekyll

Serve the blog locally with:

```
jekyll serve
```

### Serve with Docker
Just run:

```
./serve.sh
```

Stop the container with:

```
./stop.sh
```

# Template

Personal blog built with the template [Sustain](https://github.com/biomadeira/sustain/), using [Bootstrap](http://getbootstrap.com/) and powered by [Jekyll](http://jekyllrb.com/).
