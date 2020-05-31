import sys
import asyncio
import time
import aiohttp
import json
import re
from config import API_KEY

ports = {
    "Hill": 12515,
    "Jaquez": 12516,
    "Smith": 12517,
    "Campbell": 12518,
    "Singleton": 12519
}

connections = {
    "Hill":["Jaquez", "Smith"],
    "Singleton":["Jaquez", "Smith", "Campbell"],
    "Smith":["Campbell", "Hill", "Singleton"],
    "Jaquez":["Hill", "Singleton"],
    "Campbell":["Singleton", "Smith"]
}

MAX_MESSAGE = int(1e6)


def record(log):
    print(log)
    log_file.write(log+"\n")
    log_file.flush()
    
class Server:
    def __init__(self, name):
        self.name = name
        self.ip = "127.0.0.1"
        self.port = ports[name]
        self.clients = {}

    async def startup(self, reader, writer):
        data = await reader.read(MAX_MESSAGE)
        message = data.decode()
        received = time.time()
        addr = writer.get_extra_info("peername")
        record("%s recieved %s from %s" % (self.name, message, addr))
        
        sendback_message = await self.handle_message(message,received) #CHANGE THIS

        if sendback_message == "":
            return
    
        record("%s sending %s to %s" % (self.name, sendback_message, addr))

        writer.write(sendback_message.encode())
        await writer.drain()

        record("closing client socket")
        writer.close()
        
    async def run(self):
        server = await asyncio.start_server(self.startup, self.ip, self.port)

        record("%s serving on %s" % (self.name, server.sockets[0].getsockname()))
        async with server:
            await server.serve_forever()

        server.close()

        
    async def handle_message(self, message, received):
        if len(message) >= 5 and message[0:5] == "IAMAT":
            return await self.handle_IAMAT(message, received)
        elif len(message) >= 7 and message[0:7] == "WHATSAT":
            return await self.handle_WHATSAT(message, received)
        elif len(message) >= 6 and message[0:6] == "UPDATE":
            await self.handle_UPDATE(message, received)
            return ""
        else:
            return "? "+ message

    async def handle_IAMAT(self, message, received):
        parse = message.split()
        assert(parse[0] == "IAMAT")
        if len(parse) != 4:
            return "? "+ message
        clientID = parse[1]

        # Make sure coordinates are in proper form
        if re.match("[+-][0-9]*\.?[0-9]*[+-][0-9]*\.?[0-9]*", parse[2]):
            clientLocation = parse[2]
        else:
            return "? "+message

        # Make sure time can be converted to float
        try:
            clientTime = float(parse[3])
        except ValueError:
            return "? "+ message
        
        time_diff = received - clientTime
        response = "AT " + self.name + (" +" if time_diff>=0 else " ") + str(time_diff) +" "+parse[1]+" "+parse[2]+" "+parse[3]
        self.clients[clientID] = response
        flooding_string = "UPDATE " + clientID + " " + response
        asyncio.ensure_future(self.flood(flooding_string))
        return response
        

    async def handle_WHATSAT(self, message, received):
        parse = message.split()
        assert(parse[0] == "WHATSAT")
        if len(parse) != 4:
            return "? "+message
        clientID = parse[1]
        try:
            radius=int(parse[2])
            items=int(parse[3])
        except:
            return "? " +message

        radius = radius * 1000
        if clientID not in self.clients:
            return "? " + message
        ATstring = self.clients[clientID]
        parse = ATstring.split()
        m = re.match("\+?(-?[0-9]*\.?[0-9]*)\+?(-?[0-9]*\.?[0-9]*)", parse[4])
        x = m.group(1)
        y = m.group(2)
        request = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="+x+","+y+"&radius="+str(radius)+"&key="+API_KEY
        async with aiohttp.ClientSession() as session:
            async with session.get(request) as resp:
                places = await resp.json()
            #places = await fetch(session, request)
        places['results'] = places['results'][:items]
        

        return ATstring +"\n"+ json.dumps(places, indent=3)  + "\n\n"

    async def handle_UPDATE(self, message, received):
        parse = message.split()
        assert(parse[0] == "UPDATE")
        assert(len(parse) == 8)
        clientID = parse[1]
        ATstring = ' '.join(parse[2:])

        if clientID in self.clients:
            if self.clients[clientID] != ATstring:
                self.clients[clientID] = ATstring
                asyncio.ensure_future(self.flood(message))
        else:
            self.clients[clientID] = ATstring
            asyncio.ensure_future(self.flood(message))

    async def flood(self, message):
        record("Flooding the following message: %s" % (message))
        for target in connections[self.name]:
            record("Connecting to %s at port %d" % (target, ports[target]))
            try:
                reader, writer = await asyncio.open_connection(host=self.ip, port=ports[target])
                record("Connection from %s to %s successful." % (self.name, target))
                writer.write(message.encode())
                await writer.drain()
                record("%s Flooding %s to %s" % (self.name, message, target))
                writer.close()
            except:
                record("Failed to connect from %s to %s" % (self.name, target))

async def fetch(session, request):
    async with session.get(request) as resp:
        places = await resp.json()
        
def main():
    if len(sys.argv) != 2:
        print("Error: Expected two arguments. Given %d arguments.\nUsage: python3 server.py [server]" % len(sys.argv))
        exit(1)
    if sys.argv[1] not in ports:
        print("Error: Server %s does not exist.\nUsage: python3 server.py [server]" % sys.argv[1])
        exit(1)

    global log_file
    log_file = open(sys.argv[1]+"-logfile.txt", 'w+')
    log_file.truncate(0)

    server = Server(sys.argv[1])
    
    try:
        asyncio.run(server.run())
    except KeyboardInterrupt:
        pass

    log_file.close()


if __name__ == '__main__':
    main()
