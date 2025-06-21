apt-get update
apt install gnupg ca-certificates curl

# add zulu and install old jdk8
curl -s https://repos.azul.com/azul-repo.key | gpg --dearmor -o /usr/share/keyrings/azul.gpg
echo "deb [signed-by=/usr/share/keyrings/azul.gpg] https://repos.azul.com/zulu/deb stable main" | tee /etc/apt/sources.list.d/zulu.list
apt update
apt-get install zulu8-jdk

# add my stuff
apt-get install emacs-nox git htop tmux
