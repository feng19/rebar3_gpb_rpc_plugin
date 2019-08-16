all: compile

.PHONY: co compile
## 编译
co:compile
compile: include checkouts
	rebar3 compile

include:
	mkdir include

gen_proto:
	rebar3 gpb_rpc compile

checkouts:
	mkdir _checkouts
	ln -s $$PWD/.. _checkouts/rebar3_gpb_rpc_plugin