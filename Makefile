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
	@echo " make sdk - install tools into /opt/sm-sdk/"
	@echo ""
	@echo ""

.PHONY: deploy
deploy: build
	rsync -avz --delete -e 'ssh -p 2222' www/lua_modules/ tt@atreides-host.net:~/atreides-host.net/www/lua_modules/
	rsync -avz --delete -e 'ssh -p 2222' www/static/ tt@atreides-host.net:~/atreides-host.net/www/static/
	rsync -avz --delete -e 'ssh -p 2222' www/app.lua tt@atreides-host.net:~/atreides-host.net/www/app.lua
	rsync -avz --delete -e 'ssh -p 2222' www/mime.types tt@atreides-host.net:~/atreides-host.net/www/mime.types
	rsync -avz --delete -e 'ssh -p 2222' nginx.prod.conf tt@atreides-host.net:~/atreides-host.net/www/nginx.conf
	rsync -avz --delete -e 'ssh -p 2222' var/ tt@atreides-host.net:~/atreides-host.net/var/
	rsync -avz --delete -e 'ssh -p 2222' ENV.prod tt@atreides-host.net:~/atreides-host.net/ENV.prod
	ssh -p 2222 -t tt@atreides-host.net 'killall openresty && cd ~/atreides-host.net/ && source ENV.prod && cd ~/atreides-host.net/www && openresty -p . -c nginx.conf'
	@echo -e ${GREEN}done.${NC}

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

.PHONY: sdk-clean
sdk-clean:
	sudo rm -rf sdk-build-tmp

.PHONY: clean
clean: moonro-clean sdk-clean
	rm -rf www
	@echo -e ${GREEN}done.${NC}

.PHONY: sdk
sdk:
	mkdir -p sdk-build-tmp/opt/sm-sdk/bin
	sudo rm -rf /opt/sm-sdk
	
	cd sdk-build-tmp && git clone --depth 1 https://github.com/minoki/LunarML.git
	cd sdk-build-tmp/LunarML && make && sudo make install PREFIX=/opt/sm-sdk
	
	cd sdk-build-tmp && git clone --depth 1 https://github.com/melsman/mlkit.git
	cd sdk-build-tmp/mlkit && ./autobuild && ./configure --prefix=/opt/sm-sdk \
		&& make smltojs && make smltojs_basislibs && \
		sudo make install_smltojs && sudo make install_smltojs_basislibs
	
	cd /opt/sm-sdk/bin \
		&& sudo wget https://github.com/dobicinaitis/tailwind-cli-extra/releases/download/v2.1.32/tailwindcss-extra-linux-x64 \
		&& sudo chmod 755 tailwindcss-extra-linux-x64
	
	cd /opt/sm-sdk/bin \
		&& sudo wget https://github.com/azdavis/millet/releases/download/v0.14.9/millet-ls-x86_64-unknown-linux-gnu.gz \
		&& sudo gunzip millet-ls-x86_64-unknown-linux-gnu.gz && sudo chmod 755 millet-ls-x86_64-unknown-linux-gnu
	
	cd sdk-build-tmp && tar -czf ../sm-sdk-v`date +'%Y%m%d'`-1.tar.gz /opt/sm-sdk/
	@echo -e ${GREEN}done.${NC}

