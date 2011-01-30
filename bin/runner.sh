source /etc/distributest/set_hostname.sh
#The reason this is using nohup and -noshell is because with detached there are issues starting the \
#vm from an ssh session and then logging out.
#What happens is the launchctl manager dies and the Erlang vm can no longer use stuff like DirectoryService
#DirectoryService is needed to do ssh etc.
#TODO Need a better startup/shutdown script
nohup erl -name runner@$DISTRIBUTEST_LOCAL_HOSTNAME -setcookie a -noshell -pa /usr/local/distributest/ebin &