lib = File.expand_path('../lib/', __FILE__)
$:.unshift lib unless $:.include?(lib)
 
require 'distributest/version'
 
Gem::Specification.new do |s|
  s.name        = "distributest"
  s.version     = Distributest::VERSION
  s.platform    = Gem::Platform::RUBY
  s.authors     = ["Curtis Carter"]
  s.email       = ["curtis@rubyhq.com"]
  s.homepage    = "http://github.com/ccarter/distributest"
  s.summary     = "Ruby component of the Erlang based distributed test runner"
  s.description = "Uses erlectricity to communicate with the Distributest Erlang application"
 
  s.required_rubygems_version = ">= 1.3.6"
 
  s.files        = Dir.glob("lib/**/*")
  s.require_path = 'lib'
end