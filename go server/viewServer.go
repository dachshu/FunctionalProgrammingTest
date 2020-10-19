package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"log"
	"math/rand"
	"net"
	"os"
	"time"
)

const worldWidth int16 = 300
const worldHeight int16 = 300

/////////////////////////////// protocol
const csUp uint8 = 1
const csDown uint8 = 2
const csLeft uint8 = 3
const csRight uint8 = 4

const scLoginOk uint8 = 1
const scPutPlayer uint8 = 2
const scRemoverPlayer uint8 = 3
const scPos uint8 = 4

type scPacketLoginOk struct {
	size   uint8
	pkType uint8
	id     int32
}

type scPacketPutPlayer struct {
	size   uint8
	pkType uint8
	id     int32
	x      int16
	y      int16
}

type scPacketPos struct {
	size   uint8
	pkType uint8
	id     int32
	x      int16
	y      int16
}

type scPacketRomovePlayer struct {
	size   uint8
	pkType uint8
	id     int32
}

type csPacketUp struct {
	size   uint8
	pkType uint8
}

type csPacketDown struct {
	size   uint8
	pkType uint8
}

type csPacketLeft struct {
	size   uint8
	pkType uint8
}

type csPacketRight struct {
	size   uint8
	pkType uint8
}

/////////////////////////////// protocol

type posMsg struct {
	id         int32
	x          int16
	y          int16
	pHelloChan *chan helloMsg
	pByeChan   *chan int32
}

type helloMsg struct {
	id int32
	x  int16
	y  int16
}

type sectorIn struct {
	id       int32
	pPosChan *chan posMsg
}

const numS int = 15
const cliDiscon uint8 = 128
const viewRadius int16 = 4
const sectorSize int16 = 20

const maxBuffer int32 = 1024

var sinChans [numS][numS]chan sectorIn
var soutChans [numS][numS]chan int32
var sbroadChans [numS][numS]chan posMsg

func sector(col int, row int) {
	clients := make(map[int32]*chan posMsg)
	cnt := 0
	for {
		//fmt.Println(cnt, col, row, " sector execute")
		cnt++
		select {
		case inClient := <-sinChans[row][col]:
			clients[inClient.id] = inClient.pPosChan
		case outID := <-soutChans[row][col]:
			delete(clients, outID)
		case broadMsg := <-sbroadChans[row][col]:
			for _, pPosChan := range clients {
				*pPosChan <- broadMsg
			}
		}
	}
}

func sendLoginOkPacket(id int32, conn *net.Conn) {
	var packet scPacketLoginOk
	packet.id = id
	packet.size = uint8(binary.Size(packet))
	packet.pkType = scLoginOk
	var binBuf bytes.Buffer
	binary.Write(&binBuf, binary.LittleEndian, packet)
	(*conn).Write(binBuf.Bytes())
}

func sendPosPacket(id int32, x int16, y int16, conn *net.Conn) {
	var packet scPacketPos
	packet.id = id
	packet.size = uint8(binary.Size(packet))
	packet.pkType = scPos
	packet.x = x
	packet.y = y
	var binBuf bytes.Buffer
	binary.Write(&binBuf, binary.LittleEndian, packet)
	(*conn).Write(binBuf.Bytes())
}

func sendRemovePlayerPacket(id int32, conn *net.Conn) {
	var packet scPacketRomovePlayer
	packet.id = id
	packet.size = uint8(binary.Size(packet))
	packet.pkType = scRemoverPlayer
	var binBuf bytes.Buffer
	binary.Write(&binBuf, binary.LittleEndian, packet)
	(*conn).Write(binBuf.Bytes())
}

func sendPutPlayerPacket(id int32, x int16, y int16, conn *net.Conn) {
	var packet scPacketPutPlayer
	packet.id = id
	packet.size = uint8(binary.Size(packet))
	packet.pkType = scPutPlayer
	packet.x = x
	packet.y = y
	var binBuf bytes.Buffer
	binary.Write(&binBuf, binary.LittleEndian, packet)
	(*conn).Write(binBuf.Bytes())
}

func recvPacket(conn *net.Conn, packetChan *chan uint8) {
	netBuf := make([]byte, maxBuffer)
	packetBuf := make([]byte, 0, maxBuffer)
	prevSize := 0

	for {
		n, err := (*conn).Read(netBuf)
		if err != nil {
			log.Print(err)
			*packetChan <- cliDiscon
			(*conn).Close()
			break
		}
		restSize, dis := n, 0
		packetSize := uint8(0)
		if 0 < prevSize {
			packetSize = uint8(packetBuf[0])
		}
		for restSize > 0 {
			if 0 == packetSize {
				packetSize = uint8(netBuf[dis])
			}
			required := int(packetSize) - prevSize
			if restSize >= int(required) {
				packetBuf = append(packetBuf, netBuf[dis:(dis+int(required)+1)]...)

				*packetChan <- uint8(packetBuf[1])

				packetBuf = packetBuf[:0]
				restSize -= required
				dis += required
				packetSize = 0
				prevSize = 0
			} else {
				packetBuf = append(packetBuf, netBuf[dis:(dis+restSize+1)]...)
				restSize = 0
				prevSize += restSize
			}
		}

	}
}

func getViewSectors(sectors map[int]struct{}, x int16, y int16) {
	var exist = struct{}{}
	x1 := (x - viewRadius)
	if x1 < 0 {
		x1 = 0
	}
	x2 := (x + viewRadius)
	if x2 >= worldWidth {
		x2 = worldWidth - 1
	}
	y1 := (y - viewRadius)
	if y1 < 0 {
		y1 = 0
	}
	y2 := (y + viewRadius)
	if y2 >= worldHeight {
		y2 = worldHeight - 1
	}

	sectors[int((x1/sectorSize)*int16(numS)+(y1/sectorSize))] = exist
	sectors[int((x2/sectorSize)*int16(numS)+(y1/sectorSize))] = exist
	sectors[int((x1/sectorSize)*int16(numS)+(y2/sectorSize))] = exist
	sectors[int((x2/sectorSize)*int16(numS)+(y2/sectorSize))] = exist
}

func processPacket(pkType uint8, pX *int16, pY *int16) {
	switch pkType {
	case csUp:
		if 0 < (*pY) {
			(*pY)--
		}
	case csDown:
		if (worldHeight - 1) > (*pY) {
			(*pY)++
		}
	case csLeft:
		if 0 < (*pX) {
			(*pX)--
		}
	case csRight:
		if (worldWidth - 1) > (*pX) {
			(*pX)++
		}
	case cliDiscon:
		*pX = -1
		*pY = -1
	default:
		fmt.Println("Unknown Packet Type Error")
	}
}

func inMyView(myX int16, myY int16, targetX int16, targetY int16) bool {
	if (myX-viewRadius <= targetX && targetX <= myX+viewRadius) && ((myY-viewRadius) <= targetY && targetY <= (myY+viewRadius)) {
		return true
	}
	return false
}

func handleClient(id int32, sx int16, sy int16, conn net.Conn) {
	x, y := sx, sy
	packet := make(chan uint8, 200)
	pos := make(chan posMsg, 200)
	hello := make(chan helloMsg, 200)
	bye := make(chan int32, 200)

	viewList := make(map[int32]struct{})

	go recvPacket(&conn, &packet)

	sendLoginOkPacket(id, &conn)

	col, row := int(sx/sectorSize), int(sy/sectorSize)
	sinChans[row][col] <- sectorIn{id, &pos}
	viewSectors := make(map[int]struct{})
	getViewSectors(viewSectors, x, y)
	for k := range viewSectors {
		sbroadChans[k%numS][k/numS] <- posMsg{id, x, y, &hello, &bye}
	}
	sendPosPacket(id, x, y, &conn)

	for {
		select {
		case pkType := <-packet:
			processPacket(pkType, &x, &y)
			if pkType == cliDiscon {
				for k := range viewSectors {
					soutChans[row][col] <- id
					sbroadChans[k%numS][k/numS] <- posMsg{id, x, y, nil, nil}
				}
				return
			}
			newCol, newRow := int(x/sectorSize), int(y/sectorSize)
			if newCol != col || newRow != row {
				sinChans[newRow][newCol] <- sectorIn{id, &pos}
				soutChans[row][col] <- id
				col, row = newCol, newRow
			}
			for k := range viewSectors {
				delete(viewSectors, k)
			}
			getViewSectors(viewSectors, x, y)
			//////////////////////////////////////////////////////////////
			for k := range viewSectors {
				sbroadChans[k%numS][k/numS] <- posMsg{id, x, y, &hello, &bye}
			}
			//////////////////////////////////////////////////////////////

			sendPosPacket(id, x, y, &conn)
		case newCli := <-hello:
			if _, exist := viewList[newCli.id]; !exist {
				viewList[newCli.id] = struct{}{}
				sendPutPlayerPacket(newCli.id, newCli.x, newCli.y, &conn)
			}
		case outCli := <-bye:
			if _, exist := viewList[outCli]; exist {
				delete(viewList, outCli)
				sendRemovePlayerPacket(outCli, &conn)
			}
		case posCli := <-pos:
			if posCli.id == id {
				break
			}
			if _, exist := viewList[posCli.id]; exist {
				if posCli.x == -1 && posCli.y == -1 {
					delete(viewList, posCli.id)
					sendRemovePlayerPacket(posCli.id, &conn)
					break
				}
				inView := inMyView(x, y, posCli.x, posCli.y)
				if inView {
					sendPosPacket(posCli.id, posCli.x, posCli.y, &conn)
				} else {
					delete(viewList, posCli.id)
					sendRemovePlayerPacket(posCli.id, &conn)
					*(posCli.pByeChan) <- id
					break
				}

			} else {
				if posCli.x == -1 && posCli.y == -1 {
					break
				}
				inView := inMyView(x, y, posCli.x, posCli.y)
				if inView {
					viewList[posCli.id] = struct{}{}
					sendPutPlayerPacket(posCli.id, posCli.x, posCli.y, &conn)
					*(posCli.pHelloChan) <- helloMsg{id, x, y}
				}
			}
		}
	}

}

func main() {
	r := rand.New(rand.NewSource(time.Now().UnixNano()))
	var nextID int32 = 0

	for i := 0; i < numS; i++ {
		for j := 0; j < numS; j++ {
			sinChans[i][j] = make(chan sectorIn, 200)
			soutChans[i][j] = make(chan int32, 200)
			sbroadChans[i][j] = make(chan posMsg, 200)
			go sector(j, i)
		}
	}

	fmt.Println("Launching server.....")

	lisn, err := net.Listen("tcp", ":3500")
	if err != nil {
		log.Print(err)
		os.Exit(1)
	}

	for {
		conn, err := lisn.Accept()
		if err != nil {
			log.Print(err)
			conn.Close()
			break
		}

		x, y := r.Intn(int(worldWidth)), r.Intn(int(worldHeight))
		//x, y := r.Intn(int(sectorSize)), r.Intn(int(sectorSize))
		go handleClient(nextID, int16(x), int16(y), conn)
		nextID++
	}

	lisn.Close()
}
