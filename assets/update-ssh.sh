PROFILE="${1}"

ls -al $HOME/.ssh
echo "do you want to generate a new ssh key? (yes/no)"
read RESPONSE

if [ "$RESPONSE" =! "yes"]; then
  echo "exiting"
  sleep 1
  exit 0

else
  echo "please enter your email"
  read EMAIL 
  ssh-keygen -t ed25519 -C "${EMAIL}"
  echo "done"
  sleep 1

  echo "this is your ssh key, please link it to github"
  cat ~/.ssh/id_ed25519.pub
fi
