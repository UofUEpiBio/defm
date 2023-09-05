build: Dockerfile
	docker build -t gvegayon/defm:latest .

build-clang: Dockerfile-clang
	docker build -f Dockerfile-clang -t gvegayon/defm:clang .

run:
	docker run --rm -ti -v $(PWD)/..:/mnt -w/mnt gvegayon/defm:latest bash




