#pragma once

enum NodeType { WAY, BORDER, NONE, PIVOT};

class GraphNode {
	private:
		double lat;		  //phi in radians
		double lon;		  //lamde in radians
		GraphNode * pivot;	//Point B where the plane touches the sphere
		NodeType type;
		long id;
	public:
		GraphNode(long _id, double _lat, double _lon, NodeType _type = NONE);
		double Lat(){ return this->lat; };
		double Lon(){ return this->lon; };
		long Id() { return this->id; };
		long X();
		long Y();
		NodeType get_type();
		void setPivot(GraphNode _p) { this->pivot = &_p;};
};