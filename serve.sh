export JEKYLL_VERSION=3.8
port=4000
container_name=arialdomartini.github.io
docker run -d \
       --name ${container_name} \
       --volume="$PWD:/srv/jekyll" \
       -p ${port}:4000 \
       -it jekyll/jekyll:$JEKYLL_VERSION \
       jekyll serve

ip=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ${container_name})
echo "Serving at ${ip}:${port}"
