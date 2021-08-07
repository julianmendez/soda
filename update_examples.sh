#!/bin/bash

# Update manual
cp translator/src/main/resources/soda/translator/documentation/Manual.* translator/src/test/resources/soda/translator/documentation

# Update examples
cp --recursive examples/src/main/scala/soda/example translator/src/test/resources/soda/translator

