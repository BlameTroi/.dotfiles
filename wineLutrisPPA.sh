sudo dpkg --add-architecture i386
wget -nc https://dl.winehq.org/wine-builds/winehq.key
sudo apt-key add winehq.key
sudo apt-add-repository 'deb https://dl.winehq.org/wine-builds/ubuntu/ focal main'
sudo apt update
sudo apt install --install-recommends winehq-stable
# have to install deb from github
# "sudo add-apt-repository ppa:lutris-team/lutris
# "sudo apt update
# "sudo apt install lutris
