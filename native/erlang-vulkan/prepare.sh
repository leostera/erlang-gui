#!/bin/bash

VULKAN_VERSION=1.2.131.2

VULKAN_PATH=$(pwd)/_vulkan_sdk

URL="https://vulkan.lunarg.com/sdk/home#sdk/downloadConfirm/${VULKAN_VERSION}/mac/vulkansdk-macos-${VULKAN_VERSION}.tar.gz"

wget ${URL} -O ${VULKAN_PATH}

echo "Please export the following paths:"
echo ""
echo "export VULKAN_SDK=${VULKAN_PATH}/macOS"
echo "export PATH=$VULKAN_SDK/bin:$PATH"
echo "export DYLD_LIBRARY_PATH=$VULKAN_SDK/lib:$DYLD_LIBRARY_PATH"
echo "export VK_ICD_FILENAMES=$VULKAN_SDK/etc/vulkan/icd.d/MoltenVK_icd.json"
echo "export VK_LAYER_PATH=$VULKAN_SDK/etc/vulkan/explicit_layer.d"
