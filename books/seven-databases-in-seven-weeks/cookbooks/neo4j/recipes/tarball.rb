include_recipe "java"

package 'unzip'
package 'lsof'

user node.neo4j.server.user do
  comment "Neo4J Server user"
  home    node.neo4j.server.installation_dir
  shell   "/bin/bash"
  action  :create
end

group node.neo4j.server.user do
  (m = []) << node.neo4j.server.user
  members m
  action :create
end

require "tmpdir"

td          = Dir.tmpdir
tmp         = File.join(td, "neo4j-community-#{node.neo4j.server.version}.tar.gz")
tmp_spatial = File.join(td, "neo4j-spatial-#{node.neo4j.server.plugins.spatial.version}-server-plugin.zip")

remote_file(tmp) do
  source node.neo4j.server.tarball.url

  not_if "which neo4j"
end

if node.neo4j.server.plugins.spatial.enabled
  remote_file(tmp_spatial) do
    source node.neo4j.server.plugins.spatial.url
  end
end

bash "extract #{tmp}, move it to #{node.neo4j.server.installation_dir}" do
  user "root"
  cwd  "/tmp"

  code <<-EOS
    rm -rf #{node.neo4j.server.installation_dir}
    tar xfz #{tmp}
    mv --force `tar -tf #{tmp} | head -n 1 | cut -d/ -f 1` #{node.neo4j.server.installation_dir}
  EOS

  creates "#{node.neo4j.server.installation_dir}/bin/neo4j"
end

if node.neo4j.server.plugins.spatial.enabled
  bash "extract #{tmp_spatial}, move it to #{node.neo4j.server.installation_dir}/plugins" do
    user "root"
    cwd "/tmp"

    code <<-EOS
      unzip #{tmp_spatial} -d #{node.neo4j.server.installation_dir}/plugins
    EOS

    creates "#{node.neo4j.server.installation_dir}/plugins/neo4j-spatial-#{node.neo4j.server.plugins.spatial.version}.jar"
  end
end

[ node.neo4j.server.conf_dir,
  node.neo4j.server.data_dir,
  File.join(node.neo4j.server.data_dir, "log") ].each do |dir|
  directory dir do
    owner     node.neo4j.server.user
    group     node.neo4j.server.user
    recursive true
    action    :create
  end
end

[ node.neo4j.server.lib_dir,
  node.neo4j.server.data_dir,
  File.join(node.neo4j.server.installation_dir, "data"),
  File.join(node.neo4j.server.installation_dir, "system"),
  File.join(node.neo4j.server.installation_dir, "plugins"),
  node.neo4j.server.installation_dir ].each do |dir|
  bash "chown -R #{node.neo4j.server.user}:#{node.neo4j.server.user} #{dir}" do
    user "root"

    code "chown -R #{node.neo4j.server.user}:#{node.neo4j.server.user} #{dir}"
  end
end

%w(neo4j neo4j-shell).each do |f|
  link "/usr/local/bin/#{f}" do
    owner node.neo4j.server.user
    group node.neo4j.server.user
    to    "#{node.neo4j.server.installation_dir}/bin/#{f}"
  end
end

template "/etc/init.d/neo4j" do
  source "neo4j.init.erb"
  owner 'root'
  mode  0755
end

service "neo4j" do
  supports :start => true, :stop => true, :restart => true
  if node.neo4j.server.enabled
    action :enable
  else
    action :disable
  end
  subscribes :restart, 'template[/etc/init.d/neo4j]'
end

template "#{node.neo4j.server.conf_dir}/neo4j-server.properties" do
  source "neo4j-server.properties.erb"
  owner node.neo4j.server.user
  mode  0644
  notifies :restart, 'service[neo4j]'
end

template "#{node.neo4j.server.conf_dir}/neo4j-wrapper.conf" do
  source "neo4j-wrapper.conf.erb"
  owner node.neo4j.server.user
  mode  0644
  notifies :restart, 'service[neo4j]'
end

template "#{node.neo4j.server.conf_dir}/neo4j.properties" do
  source "neo4j.properties.erb"
  owner node.neo4j.server.user
  mode 0644
  notifies :restart, 'service[neo4j]'
end

template "/etc/security/limits.d/#{node.neo4j.server.user}.conf" do
  source "neo4j-limits.conf.erb"
  owner node.neo4j.server.user
  mode  0644
  notifies :restart, 'service[neo4j]'
end

ruby_block "make sure pam_limits.so is required" do
  block do
    fe = Chef::Util::FileEdit.new("/etc/pam.d/su")
    fe.search_file_replace_line(/# session    required   pam_limits.so/, "session    required   pam_limits.so")
    fe.write_file
  end
  notifies :restart, 'service[neo4j]'
end
