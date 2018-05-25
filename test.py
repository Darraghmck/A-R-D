import MySQLdb
import serial
import time

ard = serial.Serial('/dev/ttyACM0', 9600)
db = MySQLdb.connect(host="localhost",
                     user="darragh",
                     passwd="raspberry",
                     db="readings")
room = raw_input("Enter a room name: ")
cursor = db.cursor()

sql = "INSERT IGNORE INTO rooms(room) VALUES('{0}');"
cursor.execute(sql.format(room))
db.commit()
sql3 = "SELECT id FROM rooms where room = '{0}';"
cursor.execute(sql3.format(room))
id = cursor.fetchone()[0]

while 1:
    print("Waiting For Data...")
    x = ard.readline()
    print("Collecting Data...")
    #x = int(x)
    print ("Inserting Into Database...")

    sql1 = "INSERT INTO readings(reading, room, room_id) VALUES ({0}, '{1}', {2});"
    print(sql1.format(x, room, id))
    #sql = "INSERT INTO readings(reading, room) VALUES (%d, "+str(room)+")" %(x)
    cursor.execute(sql1.format(x, room, id))
    db.commit()

