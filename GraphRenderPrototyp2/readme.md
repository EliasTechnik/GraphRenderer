# GraphRenderPrototyp2

Das Programm GraphRenderer2 läd die eine JSON Datei welche WGS84 Koordinaten, Wegrelationen, Wegbreiten und Wegbeschaffenheiten enhält. Über eine Tangentialeben werden die WGS84 Koordinaten zu einer planaren Occupancy Grid Map transformiert und diese als Bild Datei abgespeichert.

## Funktionsweise

Die Knoten des Graphen enthalten GPS Koordienaten welche im WGS84 Koordinatensystem arbeiten. Das WGS84 kodiert in Längengrad und Breitengrad auf der Oberfläche eines Ellipsoiden welcher der Erde angenähert ist.

1. Die WGS84 Koordinaten werden in Kugelkoordinaten überführt. (Aktuell findet hier noch keine Korrektur des Ellipsoiden statt da ich mir nicht sicher bin ob dies notwendig ist. Theoretisch wäre das aber möglich.)

2. Der Punkt ```origin``` an dem die Tangentialebene später aufliegt (Liegt aktuell ungefähr im Clemens-Winkler-Bau) wird auf beiden Axen um 180° verschoben zum Punkt ```base```. Dadurch ligen ```base```, ```M(0,0,0)```,```origin``` auf einer Geraden.

3. Alle Punkte werden zu Karthesischen Koordinaten transformiert. Der Ursprung ligt im Erdmittelpunkt ```M(0,0,0)```.

4. Es wird eine Tangentialebene ```plane``` am Punkt ```origin``` generiert.

5. Jeder Knoten auf der Erdoberfläche wird auf die Tangentialebene ```plane``` mittels einer Geraden durch den Punkt ```base``` und dem jeweiligen Knoten projeziert.

6. Alle Knoten des Graphen liegen nun in einer gemeinsamen Ebene im dreidimmensionalen Raum. Um sie zu zweidimmensionalen Koordinaten auf einer Zeichenebene zu überführen werden zwei weitere Geraden innerhalb der Ebene Konstruiert. Dise stützen sich auf die ```Edge``` Werte welche in der Konfiguration festgelegt sind. Es wird nun für jeden Knoten in der Tangentialebene der Abstand zur linken Kante (x) und zur oberen Kante (y) gemessen.

7. Die 2d Koordinaten werden auf einen Canvas überführt und mittels Zeichenfunktionen zu Wegen verknüpft. Hier fließen Wegbreite und Kosten mit ein.

8. Die Grafik wird gespeichert (aktuell als PNG) und das Programm beendet.

## Kompilieren

Lazarus mit ObjectPascal ist Platformübergreifend und unterstützt viele Betriebssysteme und Architekturen. Getestet ist es aktuell nur unter Windows. Ubuntu stheht noch an. Am ainfachsten kompiliert man das Projekt über die IDE da sich so auch sehr bequem die Parameter anpassen lassen. Eine Kompilierung über die Komandozeile ist aber ebenfalls möglich.

## Ausführen

Da das Programm Startparameter erwartet lässt es sich nur über die Komandozeile ausführen. Navigiere mit einem Terminal deiner Wahl in das Programmverzeichnis. Mit ```GraphRenderer2.exe -c config.json -l test_data/default.json -a``` wird die Anwendung gestartet, die Konfiguration und Graphen geladen und anschließend gerendert.

### Startparameter

|Parameter|Parameter (kurz)|Wert|Funktion|
|---|---|---|---|
|```-help```|```-h```||Ruft die Hilfe auf.|
|```-load```|```-l```|```<Pfad zum Graph>```|Übergibt den Pfad zur Graphen Datei.|
|```-config```|```-c```|```<Pfad zur Konfiguration>```|Übergibt die Konfigurationsdatei. Optional, wenn nichts angegeben wird wird standartmäßig ```config.json``` geladen.|
|```-automatic```|```-a```||Verzichtet auf alle Nutzereingaben. So kann das Programm automatisch ausgeführt und nur über die Startparameter gesteuert werden.|

## Konfiguration

Allein aus den OSM Daten kann noch keine OGM generiert werden. Um Kontrolle über die Ausgabe und Anpassbarkeit zu erhalten wird eine Konfigurationsdatei geladen. Diese ist im JSON Vormat verfasst und hat folgenden Aufbau:

    {
        "WGS84":{
            "radius":"6378.137",
            "unit":"km"
        },
        "plane":{
            "origin":{
                "WGS84":{
                    "lat":"50.9248529",
                    "lon":"13.3323959"
                }
            },
            "edge":{
                "WGS84":{
                    "north_lat":"50.9263889",
                    "south_lat":"50.9230556",
                    "west_lon":"13.326666666666666",
                    "east_lon":"13.336388888888889"
                }
            }
        },
        "output":{
            "pixelwidth":"0.01",
            "unit":"m"
        }
    }

Hauptsächlich wird folgendes definiert:
- Radius der Erde am Äquator + Einheit
- Auflagepunkt der Tangentialebene im WGS84 Format
- "Kanten" der Karte. Diese definieren letztendlich den Bereich der gerendert werden soll. **Achtung: Es wird nicht geprüft ob ein Knoten innerhalb des Renderbereiches liegt. Daher sollten alle Knoten innerhalb des Bereichs liegen. Ist dem nicht so kann es zu Fehlern in der Karte oder zu einen Abbbruch des Programms kommen.**
- Ausgabeparameter:
    - Pixelbreite: Breite eines Pixels in der OGM
    - Einheit: Einheit der Pixelbreite (aktuell noch nicht implementiert, es wird immer von Meter ausgegangen)

## Bekannte Bugs und fehlerhaftes Verhalten

- Die Konfiguration in Metern führt zu fehlerhaften Werten. Vermutlich ist die Konfiguratiosndatei fehlerhaft.
- Zu hohe Auflösungen (kleine Pixelbreiten) führen zu einen ```OutOfMemoryError``` des Canvas. Das ist eine Limitierung der Zeichenbibliothek und wird sich nur durch Chunking der Karte realisieren lassen.
- Aktuell stimmt das Seitenverhältnis der OGM nicht. Es ist leicht in der Breite gestreckt. Entweder ist das die fehlende Berücksichtigung des Erdelipsoiden oder auch ein fehler der Konfigurationsparameter.
