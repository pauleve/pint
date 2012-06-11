#!/bin/sh
DYLD_LIBRARY_PATH="`dirname "$0"`" "${0}.mac" "${@}"
