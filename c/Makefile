CC:=gcc
PROJECT:=cligmm

SRC_PATHS:=src
C_SRCS:=$(foreach PATH,$(SRC_PATHS), $(wildcard $(PATH)/*.c))
C_OBJS:=${C_SRCS:.c=.o}
OBJS:=$(C_OBJS)
CURL_FLAGS:=$(shell curl-config --cflags)
CURL_LIBS:=$(shell curl-config --libs)
SANITY_FLAGS:=-Wall -pedantic -Werror
CFLAGS+= $(CURL_FLAGS) $(shell pkg-config --cflags json-c) $(SANITY_FLAGS) 
LDFLAGS += $(shell pkg-config --libs json-c)  $(CURL_LIBS)  $(shell pkg-config --libs libzip)

all: $(PROJECT)
clean: 
	@- $(RM) $(OBJS)
	@- $(RM) $(PROJECT)

$(PROJECT): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $(PROJECT) $(LDFLAGS)