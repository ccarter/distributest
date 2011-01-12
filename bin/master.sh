source /etc/distributest/set_hostname.sh
time erl -name master@$DISTRIBUTEST_LOCAL_HOSTNAME -run master -noshell -setcookie a -pa /usr/local/distributest/ebin/