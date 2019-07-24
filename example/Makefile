all: compile

.PHONY: co compile
## 编译
co:compile
compile: include
	rebar3 compile

include:
	mkdir include

gen_proto:
	rebar3 gpb_rpc compile