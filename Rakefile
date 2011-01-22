#Copied from http://seangeo.blogspot.com/2007/09/building-erlang-with-rake.html
require 'rake/clean'

INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
CLEAN.include("ebin/*.beam")
@install_destination = "/usr/local/distributest"

directory 'ebin'

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "Build Gem"
directory 'gem'
task "build_gem" do
  cd "gem"
  sh "gem build distributest.gemspec"
end

desc "Uninstall"
task :uninstall do
  puts "Removing files and folder from #{@install_destination}"
  FileUtils.rm_rf(@install_destination)
end

desc "Install Gem"
task :install_gem do
  puts "Installing gem"
  puts `gem install gem/distributest*.gem`
end

desc "Install" 
task :install do
  puts "Installing files to #{@install_destination}"
  dirs_to_copy = ["bin/", "configuration/", "ebin/", "log4erl/"]
  FileUtils.mkdir_p(@install_destination)
  dirs_to_copy.each do |copy_dir|
    FileUtils.cp_r(copy_dir, @install_destination)
  end
  FileUtils.ln_sf("#{@install_destination}/bin/master.sh", "#{@install_destination}/../bin/distributest")
  FileUtils.ln_sf("#{@install_destination}/bin/runner.sh", "#{@install_destination}/../bin/start_distributest_vm")  
  puts "!!!!!!!!!!!!!!!!!"
  puts "You should install latest distributest gem as a non root user: rake install_gem"
  puts "!!!!!!!!!!!!!!!!!"
end

desc "Compile"
task :compile => ['ebin'] + OBJ do
  Rake::Task[:build_gem].invoke
end
task :default => :compile