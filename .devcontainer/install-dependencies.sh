sudo apt-get install -y libpq-dev entr


DOWNLOAD_FILE="task_linux_amd64.deb"
DOWNLOAD_URL="https://github.com/go-task/task/releases/download/v3.35.1/$DOWNLOAD_FILE"

wget "$DOWNLOAD_URL"
sudo dpkg -i "$DOWNLOAD_FILE"
rm -rf "$DOWNLOAD_FILE"

