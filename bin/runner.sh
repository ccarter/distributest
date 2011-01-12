source /etc/distributest/set_hostname.sh
erl -name runner@$DISTRIBUTEST_LOCAL_HOSTNAME -setcookie a -detached -pa /usr/local/distributest/ebin