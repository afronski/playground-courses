include_recipe 'java'

file '/tmp/hbase.tar.gz' do
	ignore_failure true

	action :delete
end

directory '/opt/hbase' do
	owner 'vagrant'
	group 'vagrant'
	mode 0755

	action :create
end

directory '/opt/hbase/zookeeper' do
	owner 'vagrant'
	group 'vagrant'
	mode 0755

	action :create
end

remote_file '/tmp/hbase.tar.gz' do
  source 'http://ftp.ps.pl/pub/apache/hbase/stable/hbase-0.94.18.tar.gz'
  mode 0644

  action :create
end

execute 'untar-hbase' do
	user 'vagrant'
	group 'vagrant'

	command <<-EOF
		tar xfz /tmp/hbase.tar.gz
		mv hbase-0.94.18/* .
		rm -rf hbase-0.94.18/
	EOF
	cwd '/opt/hbase'

	action :run
end

template '/opt/hbase/conf/hbase-site.xml' do
	source 'hbase-config.xml.erb'

	owner 'vagrant'
	group 'vagrant'
	mode 0644

	action :create
end

execute 'start-hbase' do
	command './bin/start-hbase.sh'
	cwd '/opt/hbase'

	user 'vagrant'
	group 'vagrant'

	action :run
end