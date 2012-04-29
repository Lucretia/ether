LIB_NAME	=	ether
TARGET		=	lib$(LIB_NAME).a

SRCS		=	src/$(LIB_NAME).ads \
			src/$(LIB_NAME)-requests.ads \
			src/$(LIB_NAME)-requests.adb \
			src/$(LIB_NAME)-responses.ads \
			src/$(LIB_NAME)-responses.adb


all: $(TARGET) test

.PHONY: $(LIB_NAME)_lib.gpr apps.gpr

$(TARGET): $(LIB_NAME)_lib.gpr $(SRCS)
	gnatmake -P $(LIB_NAME)_lib.gpr

test: apps.gpr apps/test.adb $(TARGET)
	gnatmake -P apps.gpr

.PHONY: clean

clean:
	-rm obj/*
	-rm lib/*
