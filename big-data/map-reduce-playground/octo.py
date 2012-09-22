#!/usr/bin/python

from __future__ import with_statement

import asynchat
import asyncore
import base64
try:
        import bsddb
except ImportError:
        try:
                import bsddb3 as bsddb
        except ImportError:
                del sys.modules[__name__]
import bz2
import contextlib
import imp
import itertools
import logging
from optparse import OptionParser
import os
import os.path
import cPickle as pickle
import shelve
import socket
import threading
import tempfile
import time
import traceback
import UserDict


__version__ = "0.1"


class Lock(object):
    """Context-able (with-block) wrapper for a lock.
    """
    def __init__(self):
        self.__lock = threading.Lock()


    def __enter__(self):
        self.__lock.acquire()
        return self
    

    def __exit__(self, type, value, traceback):
        self.__lock.release()
        


class Task(object):
    """Represents a single task for the WorkQueue.
    """
    def __init__(self, key, value):
        self.key = key
        self.value = value


    def __eq__(self, other):
        return self.key == other.key


class WorkQueue(object):
    """Manages a list of tasks to complete."""
    def __init__(self, tasks, retry=False):
        """Creates a work queue from the given dictionary-like
        collection of tasks.
        """
        self.tasks = tasks
        self.unassigned = iter(tasks)
        self.unassigned_lock = Lock()
        self.pending = [ ]
        self.pending_lock = Lock()
        self.retry = retry


    def get_task(self):
        with self.unassigned_lock:
            try:
                key = self.unassigned.next()
            except StopIteration:
                # do we retry the pending tasks?
                if self.retry:
                    with self.pending_lock:
                        # grab a pending task if available
                        if self.pending:
                            task = self.pending.pop(0)
                            self.pending.append(task)
                            return task.key, task.value
                        else:
                            return None
                else:
                    return None
            else:
                task = Task(key, self.tasks[key])
                with self.pending_lock:
                    self.pending.append(task)
                return task.key, task.value


    def __iter__(self):
        return self


    def next(self):
        task = self.get_task()
        if not task:
            raise StopIteration("No more tasks")
        return task


    def mark_completed(self, key):
        task = Task(key, None)
        with self.pending_lock:
            try:
                self.pending.remove(task)
            except ValueError:
                pass


    def is_completed(self):
        with contextlib.nested(self.unassigned_lock, self.pending_lock):
            self.unassigned, unassigned_lookahead = \
                itertools.tee(self.unassigned)
            try:
                unassigned_lookahead.next()
            except StopIteration:
                return len(self.pending) == 0
            else:
                return False
            


class PersistantDict(UserDict.DictMixin):
    """Persistant (disk-backed) dictionary.
    """
    def __init__(self, path=None):
        self.logging = logging.getLogger(self.__class__.__name__)
        if not path:
            fd, path = tempfile.mkstemp(".shelve", "octo_", text=False)
            os.close(fd)
        self.logging.debug("Shelve DB: %s", path)
        self._db = bsddb.hashopen(path, 'n')
        self._shelve = shelve.BsdDbShelf(self._db, protocol=2,
                                         writeback=True)


    def __del__(self):
        self._shelve.close()
        self._db.close()
        

    def __delitem__(self, key):
        pkey = pickle.dumps(key)
        self._shelve.__delitem__(pkey)


    def __getitem__(self, key):
        pkey = pickle.dumps(key)
        return self._shelve.__getitem__(pkey)


    def __setitem__(self, key, value):
        pkey = pickle.dumps(key)
        self._shelve.__setitem__(pkey, value)
        self._shelve.sync()


    def keys(self):
        return (pickle.loads(pkey) for pkey in self._shelve.keys())


    def sync(self):
        self._shelve.sync()


    def append(self, key, value):
        pkey = pickle.dumps(key)
        self._shelve.setdefault(pkey, []).append(value)
        self._shelve.sync()
        
    

class TaskServer(object):
    """Generic task server."""
    def __init__(self, tasks, taskfile, taskretry=False,
                 intermediate=None, final=None,
                 **options):
        self.logging = logging.getLogger(self.__class__.__name__)
        self.taskfile = taskfile
        self.taskretry = taskretry
        self.workqueue = WorkQueue(tasks, retry=taskretry)
        self.intermediate = PersistantDict(intermediate)
        self.final = PersistantDict(final)
        self.handle_results = self.handle_map_results
        self.handle_completed = self.handle_map_completed
        self.phase = "map"
        self.lock = Lock()


    def __del__(self):
        del self.intermediate
        del self.final


    def next_task(self):
        """Get the next task from the queue.
        """
        try:
            key, value = self.workqueue.next()
        except StopIteration:
            self.logging.info("No more tasks")
            return None
        else:
            self.logging.debug("Next task: %s", key)
            return self.phase, key, value

    
    def handle_map_results(self, key, value):
        """Store the results of a map task.
        """
        with self.lock:
            self.logging.debug("Map results: %s", value)
            i_key, i_value = value
            self.intermediate.append(i_key, i_value)
            self.intermediate.sync()


    def handle_map_completed(self, key):
        """Mark a map task as completed.
        """
        self.logging.debug("Map complete")
        with self.lock:
            self.workqueue.mark_completed(key)
            if self.workqueue.is_completed():
                self.logging.info("Map phase complete")
                self.process_intermediate()


    def process_intermediate(self):
        """Process the intermediate map results and prepare for the reduce
        phase.
        """
        self.workqueue = WorkQueue(self.intermediate, retry=self.taskretry)
        self.handle_results = self.handle_reduce_results
        self.handle_completed = self.handle_reduce_completed
        self.phase = "reduce"


    def handle_reduce_results(self, key, value):
        """Store the results of a reduce task.
        """
        with self.lock:
            self.logging.debug("Reduce results: %s", value)
            self.final[key] = value


    def handle_reduce_completed(self, key):
        """Mark a reduce task as completed.
        """
        self.logging.debug("Reduce complete")
        with self.lock:
            self.workqueue.mark_completed(key)
            if self.workqueue.is_completed():
                self.logging.info("Reduce phase complete")
                self.stop()

                
    def start(self):
        """Start the server.
        """
        pass


    def stop(self):
        """Stop the server.
        """
        pass


    
class TaskClient(object):
    """Generic task client."""
    def __init__(self, **options):
        self.logging = logging.getLogger(self.__class__.__name__)
        self.id = hash(socket.gethostname())
        self.logging.info("id: %s", self.id)
        

    def start(self, mapfn, reducefn):
        """Start the task client.
        """
        pass

        
    
DEFAULT_CHAT_PORT = 18273

class ChatProtocol(asynchat.async_chat):
    """Base class for the simple Chat protocol.

    This handles buffering of network traffic and calls the abstract
    received() method.
    """
    def __init__(self):
        self.set_terminator("\n")
        self.buffer = []
    
    def collect_incoming_data(self, data):
        self.buffer.append(data)

    def found_terminator(self):
        data = "".join(self.buffer)
        self.buffer = []
        self.received(data)
        
    def decode_data(self, data):
        return pickle.loads(bz2.decompress(base64.b64decode(data)))

    def encode_data(self, data):
        return base64.b64encode(bz2.compress(pickle.dumps(data, 2)))


    
class ChatTaskServer(TaskServer,asyncore.dispatcher):
    """Simple socket-based server for tasks.
    """
    class ChatServerChannel(ChatProtocol):
        def __init__(self, conn, taskserver):
            self.logging = logging.getLogger(self.__class__.__name__)
            asynchat.async_chat.__init__(self, conn)
            ChatProtocol.__init__(self)
            self.taskserver = taskserver

        def command_get(self, data):
            """Get the next available task.
            """
            task = self.taskserver.next_task()
            if task:
                self.logging.info("Assigning %s task %s", task[0], task[1])
            data = self.encode_data(task)
            self.push(data)
            self.push("\n")

        def command_post(self, data):
            """Post a single key-value pair as a result.
            """
            # split data into task and result
            key, value = self.decode_data(data)
            self.logging.debug("Putting %s, %s", key, value)
            self.taskserver.handle_results(key, value)
            self.push("ok\n")
            
        def command_posts(self, data):
            """Post a list of key-value pairs as results.

            Note that this method does not take a dictionary.
            """
            data_dict = self.decode_data(data)
            for key, value in data_dict:
                self.logging.debug("Putting %s, %s", key, value)
                self.taskserver.handle_results(key, value)
            self.push("ok\n")

        def command_completed(self, data):
            """Indicate that work on a given key is complete.
            """
            key = self.decode_data(data)
            self.logging.debug("Completed %s", key)
            self.taskserver.handle_completed(key)
            self.push("ok\n")
            
        def command_taskfile(self, data):
            """Retrieve the taskfile.
            """
            with open(self.taskserver.taskfile) as f:
                taskfilecontents = f.read()
            data = self.encode_data(taskfilecontents)
            self.push(data)
            self.push("\n")
            
        def command_invalid(self, data):
            """Invalid command.
            """
            self.logging.error("Invalid command: %s", comand)
            self.push("error\n")
            
        def received(self, data):
            """Handles a request of the form:
            
            command,id,[data]
            
            where the data is optional, but the 2nd comma seperator is
            not.

            See .command_ methods for accepted commands.
            
            Data is encoded via the pickle and base64 modules.
            """
            command, id, data = data.split(",", 2)
            command = command.lower()
            id = int(id)
            self.logging.debug("Command: %s From: %s", command, id)
            # act on the command
            {"get":       self.command_get,
             "post":      self.command_post,
             "posts":     self.command_posts,
             "completed": self.command_completed,
             "taskfile":  self.command_taskfile,
            }.get(command, self.command_invalid)(data)
            self.close_when_done()
            

    def __init__(self, tasks, taskfile,
                 host="", port=DEFAULT_CHAT_PORT):
        self.logging = logging.getLogger(self.__class__.__name__)
        asyncore.dispatcher.__init__(self)
        TaskServer.__init__(self, tasks, taskfile)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.logging.info("Listening on %s:%s", host, port)
        self.bind((host, port))
        self.listen(5)


    def handle_accept(self):
        channel, address = self.accept()
        self.logging.debug("Accepted connection from %s:%s", *address)
        ChatTaskServer.ChatServerChannel(channel, self)


    def start(self):
        self.logging.info("Starting server...")
        asyncore.loop()
        self.logging.info("Server stopped")


    def stop(self):
        self.logging.info("Stopping server...")
        self.close()
        


DEFAULT_CHAT_POLL = 60
DEFAULT_CHAT_RETRY = 3
DEFAULT_CHAT_CLIENT_BUFFER = 1024

class ChatClient(TaskClient):
    """Simple socket-based client for tasks.
    """
    class ChatClientChannel(ChatProtocol):

        def __init__(self, host, port, id, taskclient):
            self.logging = logging.getLogger(self.__class__.__name__)
            asynchat.async_chat.__init__(self)
            ChatProtocol.__init__(self)
            self.decode = False
            self.id = id
            self.data = None
            self.taskclient = taskclient
            # connect
            self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
            self.logging.debug("Connecting to %s:%s", host, port)
            self.connect((host, port))

        def handle_connect(self):
            self.logging.debug("Connected")
            self.taskclient.retry = 0

        def handle_expt(self):
            self.logging.error("Connection failed")
            self.close()
            self.taskclient.retry += 1

        def get_task(self):
            self.decode = True
            self.push("get,%s,\n" % self.id)

        def get_taskfile(self):
            self.decode = True
            self.push("taskfile,%s,\n" % self.id)

        def post_result(self, key, value):
            self.decode = False
            data = self.encode_data((key, value))
            self.push("post,%s,%s\n" % (self.id, data))

        def post_results(self, results):
            self.decode = False
            data = self.encode_data(results)
            self.push("posts,%s,%s\n" % (self.id, data))

        def post_completed(self, key):
            self.decode = False
            data = self.encode_data(key)
            self.push("completed,%s,%s\n" % (self.id, data))

        def received(self, data):
            if self.decode:
                self.data = self.decode_data(data)
            else:
                self.data = data
            self.handle_close()

    
    def __init__(self,
                 host="localhost", port=DEFAULT_CHAT_PORT,
                 poll=DEFAULT_CHAT_POLL, maxretries=DEFAULT_CHAT_RETRY,
                 buffersize=DEFAULT_CHAT_CLIENT_BUFFER):
        super(ChatClient, self).__init__()
        self.logging = logging.getLogger(self.__class__.__name__)
        self.host = host
        self.port = port
        self.poll = poll
        self.maxretries = maxretries
        self.retry = 0
        self.results_buffer = [ ]
        self.buffersize = buffersize


    def channel(self, method, *args):
        channel = ChatClient.ChatClientChannel(self.host, self.port,
                                               self.id, self)
        method(channel, *args)
        asyncore.loop()
        return channel.data


    def start(self, mapfn, reducefn):
        while True:
            # get a task
            data = self.channel(ChatClient.ChatClientChannel.get_task)
            if data:
                # work on the task
                phase, key, value = data
                self.logging.info("%s on %s", phase, key)
                if phase == "map":
                    for i_key, i_value in mapfn(key, value):
                        self.post_result(key, (i_key, i_value))
                    self.post_completed(key)
                elif phase == "reduce":
                    f_value = reducefn(key, value)
                    self.post_result(key, f_value)
                    self.post_completed(key)
            else:
                # no task
                if self.retry >= self.maxretries:
                    self.logging.error("Maximum retries; quitting...")
                    break
                self.logging.info("No work; sleeping...")
                time.sleep(self.poll)


    def stop(self):
        self.close()


    def post_completed(self, key):
        # if buffers aren't empty, flush them
        if self.results_buffer:
            self.post_results()
        self.channel(ChatClient.ChatClientChannel.post_completed, key)
        
    
    def post_results(self):
        self.logging.debug("Reporting multiple results...")
        buffer = self.results_buffer
        self.results_buffer = []
        self.channel(ChatClient.ChatClientChannel.post_results, buffer)
        

    def post_result(self, key, value):
        # check if we are buffering
        if self.buffersize:
            # buffer results
            self.results_buffer.append((key, value))
            # if buffer is full, flush them
            if len(self.results_buffer) >= self.buffersize:
                self.post_results()
        else:
            # report results
            self.logging.debug("Reporting results for %s...", key)
            self.channel(ChatClient.ChatClientChannel.post_result,
                         key, value)
        
    
    def get_taskfile(self):
        data = self.channel(ChatClient.ChatClientChannel.get_taskfile)
        return data
    


IPC_HANDLERS = {'simple': (ChatTaskServer, ChatClient),
                }
    
    
def parse_options():
    parser = OptionParser(usage="""%prog [options] server TASKFILE
or
%prog [options] client (TASKFILE | SERVER_ADDRESS)""")
    parser.set_defaults(type="simple", verbose=False)
    parser.add_option("-t", "--type", help="IPC type (-t help for more info)")
    parser.add_option("-p", "--port", help="port to use", type="int")
    parser.add_option("-v", "--verbose", help="print more detailed messages",
                      action="store_true")
    options, args = parser.parse_args()
    if options.type.lower() == "help" \
            or options.type.lower() not in IPC_HANDLERS.keys():
        parser.error("Supported IPC types: " + ", ".join(IPC_HANDLERS.keys()))
    if len(args) > 2:
        parser.error('invalid number of arguments')
    args[0] = args[0].lower()
    if args[0] not in ('client', 'server'):
        parser.error('argument must be either "server" or "client"')
    else:
        mode = args[0]
    taskfile = args[1]
    extraoptions = {}
    if options.port:
        extraoptions["port"] = options.port
    return (mode, taskfile, options.type.lower(), options.verbose,
            extraoptions)


def prepare_taskfile(taskfile):
    """Attempt to load the taskfile as a module.
    """
    path = os.path.dirname(taskfile)
    taskmodulename = os.path.splitext(os.path.basename(taskfile))[0]
    logging.info("Loading task file %s from %s", taskmodulename, path)
    fp, pathname, description = imp.find_module(taskmodulename, [path])
    try:
        return imp.load_module(taskmodulename, fp, pathname, description)
    finally:
        if fp:
            fp.close()


def do_server(taskfile, type, **options):
    """Starts the server.

    The taskfile is loaded and tasks retrieved. The server is then
    started with the specified tasks, taskfile, and type. The server
    first distributes map tasks then reduce tasks and then finalizes
    the results.
    
    Additional options are passed through to the specific server
    constructor.

    See IPC_HANDLERS for supported types and the respective servers
    for options.
    """
    taskmodule = prepare_taskfile(taskfile)
    # prepare tasks
    tasks = taskmodule.source
    # start task/data server
    taskserver = IPC_HANDLERS[type][0](tasks, taskfile, **options)
    taskserver.start()
    # tasks completed, run final
    if hasattr(taskmodule, "final"):
        for key in sorted(list(taskserver.final.keys())):
            taskmodule.final(key, taskserver.final[key])

    
def do_client(taskfile, type, **options):
    """Starts the client.

    Attempts to load/retrieve the taskfile. The client is then started
    with the work function from the taskfile and specified type.

    Additional options are passed through to the specific client
    constructor.

    See IPC_HANDLERS for supported types and the respective clients
    for options.
    """
    # try opening the file
    f = None
    try:
        f = open(taskfile)
    except IOError:
        # try downloading the file using the ipc handler
        server = taskfile
        taskclient = IPC_HANDLERS[type][1](host=server, **options)
        taskfilecontents = taskclient.get_taskfile()
        if taskfilecontents:
            # write task file to temporary file
            fd, taskfile = tempfile.mkstemp(".py", "octo_", text=True)
            try:
                os.write(fd, taskfilecontents)
            finally:
                os.close(fd)
            taskmodule = prepare_taskfile(taskfile)
        else:
            logging.critical("Unable to retrieve task file")
            return
    else:
        taskmodule = prepare_taskfile(taskfile)
        server = getattr(taskmodule, "server", "localhost")
        taskclient = IPC_HANDLERS[type][1](host=server, **options)
    finally:
        if f:
            f.close()
    # start the task client
    taskclient.start(taskmodule.mapfn, taskmodule.reducefn)
    

def main():
    # parse arguments
    mode, taskfile, type, verbose, options = parse_options()
    # set up logging
    if verbose:
        logging.basicConfig(logging=logging.DEBUG,
                            format="%(asctime)s [%(levelname)s] %(name)s: %(message)s")
        logging.getLogger("").setLevel(logging.DEBUG)
    else:
        logging.basicConfig(logging=logging.INFO,
                            format="[%(levelname)s] %(message)s")
        logging.getLogger("").setLevel(logging.INFO)
    # are we server or client?
    {'server': do_server,
     'client': do_client,
     }[mode](taskfile, type, **options)
    

if __name__ == "__main__":
    main()
