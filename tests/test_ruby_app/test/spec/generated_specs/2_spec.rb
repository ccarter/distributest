require File.dirname(__FILE__) + "/../spec_helper.rb"
5.times do
  describe "2 file spec" do
    it "should pass" do
      sleep(4)
      1.should == 1
    end
  end
end