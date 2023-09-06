build: Dockerfile
	docker build -t gvegayon/defm:latest .

build-clang: Dockerfile-clang
	docker build -f Dockerfile-clang -t gvegayon/defm:clang .

build-address: Dockerfile-w-address
	docker build -t gvegayon/defm:address -f Dockerfile-w-address .

run:
	docker run --rm -ti -v $(PWD)/..:/mnt -w/mnt gvegayon/defm:latest bash


run-address:
	docker run --rm -ti -v $(PWD)/..:/mnt -w/mnt gvegayon/defm:address bash