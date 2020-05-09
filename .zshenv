export EDITOR=vim

# Golang
export GOPATH=${HOME}/work/golang
export PATH=${GOPATH}/bin:${PATH}

# for Debian
export DEBEMAIL="rare@tirasweel.org"
export DEBFULLNAME="Katsuki Kobayashi"
alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"

# cargo & pip
export PATH=${HOME}/.cargo/bin:${HOME}/.local/bin:${PATH}

# SDKMAN
source ${HOME}/.sdkman/bin/sdkman-init.sh

# cask
export PATH=${HOME}/.cask/bin:${PATH}

# gem
export GEM_HOME=${HOME}/.gem
export PATH=${GEM_HOME}/bin:${PATH}

# for RISC-V
export PATH=/opt/riscv32imac/bin:${PATH}
