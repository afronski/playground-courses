require 'serverspec'

include Serverspec::Helper::Exec
include Serverspec::Helper::DetectOS

RSpec.configure do |config|
  config.before :all do
    config.path = '/sbin:/usr/sbin'
  end
end

describe file('/opt/hbase') do
  it { should be_directory }
  it { should be_mode 755 }
  it { should be_owned_by 'vagrant' }
end

describe file('/opt/hbase/zookeeper') do
  it { should be_directory }
  it { should be_mode 755 }
  it { should be_owned_by 'vagrant' }
end

describe file('/opt/hbase/conf/hbase-site.xml') do
  it { should be_file }
  it { should be_mode 644 }
  it { should be_owned_by 'vagrant' }
end

describe port(2181) do
  it { should be_listening }
end