# conq

## conqiler

Clang v17.0.0+ needs to be installed on the system
```bash
# uninstall old versions of llvm/clang
sudo apt uninstall "*clang*"
sudo apt uninstall "*llvm*"
sudo apt uninstall "*mlir*"
sudo apt uninstall "*polly*"
# install llvm 17
wget https://apt.llvm.org/llvm.sh
sudo llvm.sh 17 all
rm llvm.sh
# setup llvm 17 env including mlir
sudo ln /usr/bin/llvm-config-17 /usr/bin/llvm-config
sudo apt install libmlir-17-dev
```

## conq-er