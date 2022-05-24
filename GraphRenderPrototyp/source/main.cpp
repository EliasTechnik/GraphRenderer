#include <iostream>
#include "tinyxml2.h" //xml parser





int main() {
    XMLDocument doc;
    doc.LoadFile("test_data/xml/default.xml");
    XMLElement* XMLgraph = doc.FirstChildElement("graph");

    XMLNode* aWay;
    while (XMLgraph->FirstChild()!=NULL) {
        aWay = XMLgraph->FirstChild();
        while (aWay->FirstChild() != NULL) {
            XMLNode* nd = aWay->FirstChild();
            std::cout << nd->GetUserData();
            std::cout << "\n\r";
            aWay->DeleteChild(nd);
        }
        XMLgraph->DeleteChild(aWay);
    }





    std::cout << "Hello World!";
    return 0;
}