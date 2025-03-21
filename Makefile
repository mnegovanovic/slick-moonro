GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

.PHONY: all
all:
	@echo ""
	@echo ""
	@echo " make build - compile everything into local 'www' directory"
	@echo " make run - build and run openresty"
	@echo " make slick - standalone compile of the back-end"
	@echo " make moonro - standalone compile of the front-end"
	@echo ""
	@echo ""

.PHONY: deploy
deploy: build
	rsync -avz --delete www/lua_modules/ tt@atreides-host.net:~/atreides-host-website/www/lua_modules/
	rsync -avz --delete www/static/ tt@atreides-host.net:~/atreides-host-website/www/static/
	rsync -avz --delete www/app.lua tt@atreides-host.net:~/atreides-host-website/www/app.lua
	rsync -avz --delete www/mime.types tt@atreides-host.net:~/atreides-host-website/www/mime.types
	rsync -avz --delete www/nginx.conf tt@atreides-host.net:~/atreides-host-website/www/nginx.conf
	@echo "done."

.PHONY: slick
slick:
	mkdir -p www
	lunarml compile --lib --luajit -o www/app.lua src/back/back.mlb
	@echo -e ${GREEN}done.${NC}

.PHONY: moonro
moonro:
	rm -rf www/static
	mkdir -p www/static
	
	cd src/front && smltojs -basislib front.mlb && grep -oP '(?<=src=")[^"]*.js' run.html | xargs cat > app.js
	cp src/front/app.js www/static/
	
	cd src/front && tailwindcss-extra-linux-x64 -i static/css/tw-init.css -o static/css/tw.css
	
	cp -r src/front/static/* www/static/
	
	rm -rf src/front/run.html
	rm -rf src/front/app.js
	rm -rf src/front/static/css/tw.css
	find src/ -type d -name "MLB" | xargs rm -rf
	@echo -e ${GREEN}done.${NC}

.PHONY: moonro-clean
moonro-clean:
	rm -rf src/front/run.html
	rm -rf src/front/app.js
	rm -rf src/front/static/css/tw.css
	find src/ -type d -name "MLB" | xargs rm -rf
	@echo -e ${GREEN}done.${NC}

.PHONY: build
build: clean slick moonro
	mkdir -p www/logs
	mkdir -p www/static

ifeq (,$(wildcard ./lua_modules/.))
	./install_local_rocks
endif
	cp -r lua_modules www/
	cp nginx.conf www/
	cp mime.types www/
	
	@echo -e ${GREEN}done.${NC}

.PHONY: run
run: build
	cd www && openresty -p . -c nginx.conf
	@echo -e ${GREEN}done.${NC}

.PHONY: clean
clean:
	rm -rf www
	@echo -e ${GREEN}done.${NC}

