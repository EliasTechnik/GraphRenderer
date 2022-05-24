#include "graph.h"

GraphNode::GraphNode(long _id, double _lat, double _lon, NodeType _type) {
	this->id = _id;
	this->lat = _lat;
	this->lon = _lon;
	this->type = _type;
	this->pivot = this;
}

long GraphNode::X() {
	//todo
	return 0;
}
long GraphNode::Y() {
	//todo
	return 0;
}
NodeType GraphNode::get_type() {
	return this->type;
}