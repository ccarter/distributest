#/bin/bash
#This is per runner
export RAILS_ENV=test
export DB_PREFIX=$1
rake db:create
rake db_steps