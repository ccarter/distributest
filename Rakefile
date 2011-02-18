#Parts copied from http://seangeo.blogspot.com/2007/09/building-erlang-with-rake.html
require 'rake/clean'

INCLUDE = "include"
ERLC_FLAGS = "-I#{INCLUDE} +warn_unused_vars +warn_unused_import"

SRC = FileList['src/*.erl']
TEST_SRC = FileList['tests/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
TEST_OBJ = TEST_SRC.pathmap("%{tests,tests}X.beam")
CLEAN.include("ebin/*.beam")
@install_destination = "/usr/local/distributest"

directory 'ebin'

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

#Test output from http://barrymitchelson.com/blog/tags/rake.html
#Compiles tests and then application with export_all
desc "Run Distributest Tests"
task "run_distributest_tests" => ['tests'] + TEST_OBJ do
  #Compile app with export all
  ERLC_FLAGS = ERLC_FLAGS + " +export_all "
  Rake::Task[:compile].invoke
  modules = OBJ.map {|o| File.basename(o, ".beam") }

  output = `erl \
    -noshell \
    -pa #{File.dirname(__FILE__) + '/ebin'} \
    -eval 'eunit:test([#{modules.join(",")}], [verbose])' \
    -s init stop`

  output.each_line do |line|
    case line
      when /= (EUnit) =/
        print line.gsub($1, green($1))
      when /\*failed\*/
        print red(line)
      when /(\.\.\..*ok)/
        print line.gsub($1,green($1))
      when /Failed:\s+(\d+)\.\s+Skipped:\s+(\d+)\.\s+Passed:\s+(\d+)\./
        puts "#{red("Failed: #{$1}")} Skipped: #{$2} #{green("Passed: #{$3}")}"
      when/(All \d+ tests passed.)/
        print green(line)
    else
      print line
    end
  end
end

def green(text)
  "\e[32m#{text}\e[0m"
end

def red(text)
  "\e[31m#{text}\e[0m"
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
  dirs_to_copy = ["bin/", "ebin/"]
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

task :compile_tests => ['tests'] + TEST_OBJ

desc "Compile"
task :compile => ['ebin'] + OBJ do
  Rake::Task[:build_gem].invoke
end
task :default => :compile