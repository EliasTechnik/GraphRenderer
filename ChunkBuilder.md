# ChunkBuilder

## Hauptaufgabe

Die vom Graphrender erzeugten Subchunks der Globalmap müssen während der Laufzeit zur Globalmap zusammengesetzt werden. Dabei wird aus 3*3 Subchunks ein Chunk zusammengesetzt. Der Standort des Roboters sollte sich immer innerhalb des mittleren Subchunks befinden. Verlässt der Roboter diesen Subchunk wird der Chunk neu zusammengesetzt sodas die Position des Roboters wieder im mittleren Subchunk liegt.

## Notwendigkeiten

Um die o.g. Aufgabe zu erfüllem muss das Programm folgende Funktionalitäten mitbringen:

- Laden von 9 Subchunks aus einen vordefinierten Ordner.
- Zusammensetzen der 9 Subchunks zu einen Bild.
- Speichern des Bildes an eine Zieladresse
- Einlesen der Konfigurationsdatei ```subchunk_index.json``` welche die Koordinaten der Subchunks sowie deren Pixelbreite und Kantenlänge enthält.
- Eingabemöglichkeit der Position des Roboters in XY-Koordinaten. (siehe unten)

## Ausführung

Besagter Chunkbuilder kann als Script, welches Komandozeilen Startparameter entgegennimmt, ausgeführt werden oder als ROS Node welche auf dem 2D-Positions Topic* lauscht und selbständig erkennt wann ein neuer Chunk gebaut werden muss.

**Dieses Topic existiert aktuell noch nicht und könnte in Zukunft auch anders heißen. Geplant ist eine Übersetzungseinheit welche die GPS Daten des Sensors in Karthesische Koordinaten überführt und diese Information kontinuierlich published.*

## Subchunks

Der GraphRender erzeugt im Ordner ```output/``` die Subchunks der Globalmap. Das können je nach Konfiguration zwischen 9 und 460 Einzelbilder sein. Jeder Subchunk ist folgendermaßen benannt: ```subchunk_SX_SY.png```. ```SX``` ist die X und ```SY``` die Y Position des Subchunks im SUbchunkgitter. Die pixelgenaue Position der linken oberenb Ecke des jewiligen Subchunks ist in ```subchunk_index.json``` gespeichert:

    {
        "subchunks": {
            "size": "1000",
            "subchunk_0_0.png": {
                "x": "0",
                "y": "0"
            },
            "subchunk_0_1.png": {
                "x": "0",
                "y": "1000"
            },
            "subchunk_0_2.png": {
                "x": "0",
                "y": "2000"
            },
            "subchunk_0_3.png": {
                "x": "0",
                "y": "3000"
            },
            "subchunk_0_4.png": {
                "x": "0",
                "y": "4000"
            },
            
            .

            .

            .

            "subchunk_22_19.png": {
                "x": "22000",
                "y": "19000"
            }
        },
        "context": {
            "WGS84": {
                "semi-major-axis": "6378137.0",
                "semi-minor-axis": "6356752.314245",
                "baseunit": "m"
            },
            "plane": {
                "origin": {
                    "WGS84": {
                        "lat": "50.9248529",
                        "lon": "13.3323959"
                    }
                },
                "edge": {
                    "WGS84": {
                        "north_lat": "50.9263889",
                        "south_lat": "50.9230556",
                        "west_lon": "13.326666666666666",
                        "east_lon": "13.336388888888889"
                    }
                }
            },
            "output": {
                "pixelwidth": "0.01",
                "subchunksize": "10"
            }
        }
    }

- ```size``` gibt die Kantenlänge eines Subchunks in Pixel an.
- ```Pixelwidth```die Pixelbreite in Metern,
- ```subchunksize``` die Breite eines Subchunks in Metern.

Die Angaben unter ```context``` sind für die geforderte Anwendung nicht relevant. Die dienen ldediglich der Dokumentation und Reproduzierbarkeit.