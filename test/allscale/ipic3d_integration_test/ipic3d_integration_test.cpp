#include <cassert>
#include <sstream>
#include <vector>

#include <cmath>

#include "allscale/ipic3d.h"
#include "allscale/utils/grid.h"
#include "allscale/utils/static_grid.h"
#include "allscale/utils/io.h"

#include "utils/string_utils.h"
#include "parec/ops.h"

	using namespace allscale;
	using namespace allscale::utils;

	template<typename T> struct Vector {
		T x;
		T y;
		T z;
	};

	struct Particle {
		double x,y,z;			// position (absolute - TODO: relative to cell center)
		double dx,dy,dz;		// velocity
		double q;			// charge divided over the mass of spacies
		double vxstar, vystar, vzstar;  // auxiliary parameters for the Boris mover
	};

	struct FieldCell {
		Vector<double> E;		// electric field
		Vector<double> B;		// magnetic field
	};

	using Field = utils::Grid<FieldCell,3>;	// a 3D grid of field cells

	struct DensityCell {
		double rho;			// charge density
		double J;			// current density
	};

	using Density = utils::Grid<DensityCell,3>;	// a 3D grid of density cells

	/**
	 * The structure of a single cell, forming a container for a set of particles
	 * located within a confined area of space.
	 */
	struct Cell {

		using Coord = utils::Coordinate<3>;

		// the cell position
		double x, y, z;

		// the cell grid spacing
		double dx, dy, dz;

		// the list of local particles
		std::vector<Particle> particles;

		/**
		 * Requires this cell, being located at position `pos`, to project the effect
		 * of its contained particles to the density grid. Contributions are stored
		 * within the given contributions grid
		 */
		void projectToDensityField(const Coord& pos, utils::Grid<DensityCell,3>& contributions) const {

			// quick-check
			if (particles.empty()) return;		// nothing to contribute

			// init aggregated densities of neighboring cells
			DensityCell res[2][2][2];
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						res[i][j][k].rho = 0.0;
						res[i][j][k].J = 0.0;
					}
				}
			}

			// aggregate particles
			parec::pfor(particles, [&](const Particle& p) {
				for(int i=0; i<2; i++) {
					for(int j=0; j<2; j++) {
						for(int k=0; k<2; k++) {
							// TODO: add an actual interpolation
							res[i][j][k].rho += p.q;
							res[i][j][k].J += p.q;
						}
					}
				}
			});

			// write contributions to contributions grid
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						Coord cur = (pos * 2)  + Coord{i,j,k};
						contributions[cur] = res[i][j][k];
					}
				}
			}
		}

		/**
		 * Interpolation of particles to grid
		 *
		 * Requires this cell, being located at the position `pos`, to project the effect
		 * of its contained particles to the density grid. Contributions are stored
		 * within the given contributions grid
		 */
		void interP2G(const Coord& pos, utils::Grid<DensityCell,3>& contributions) const {

			// quick-check
			if (particles.empty()) return;		// nothing to contribute

			// init aggregated densities of neighboring cells
			DensityCell res[2][2][2];
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						res[i][j][k].rho = 0.0;
						res[i][j][k].J = 0.0;
					}
				}
			}

			// aggregate particles
			parec::pfor(particles, [&](const Particle& p) {
				for(int i=0; i<2; i++) {
					for(int j=0; j<2; j++) {
						for(int k=0; k<2; k++) {
							// TODO: add an actual interpolation
							res[i][j][k].rho += p.q;
							res[i][j][k].J += p.q;
						}
					}
				}
			});

			// write contributions to contributions grid
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						Coord cur = (pos * 2)  + Coord{i,j,k};
						contributions[cur] = res[i][j][k];
					}
				}
			}
		}

		/**
		 * This method is updating the position of all particles within this cell for a single
		 * time step, thereby considering the given field as a driving force. Particles
		 * leaving the cell are submitted via channels to neighboring cells.
		 *
		 * @param pos the coordinates of this cell in the grid
		 * @param field the most recently computed state of the surrounding force fields
		 * @param transfers a grid of buffers to send particles to
		 */
		void moveParticles(const Coord& pos, const Field& field, utils::Grid<std::vector<Particle>,3>& transfers) {

			// quick-check
			if (particles.empty()) return;

			// extract forces
			Vector<double> E[2][2][2];
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						Coord cur({pos[0]+i,pos[1]+j,pos[2]+k});
						E[i][j][k] = field[cur].E;
					}
				}
			}

			// update particles
			parec::pfor(particles, [&](Particle& p){

				// compute forces
				double fx, fy, fz;
				fx = fy = fz = 0.0;
				for(int i=0; i<2; i++) {
					for(int j=0; j<2; j++) {
						for(int k=0; k<2; k++) {
							fx += E[i][j][k].x * p.q;		// or whatever formula is required
							fy += E[i][j][k].y * p.q;		// or whatever formula is required
							fz += E[i][j][k].z * p.q;		// or whatever formula is required
						}
					}
				}

				// update speed
				p.dx += fx;		// assuming mass = 1 unit
				p.dy += fy;
				p.dz += fz;

				// move particle
				p.x += p.dx;
				p.y += p.dy;
				p.z += p.dz;

			});

			// get buffers for particles to be send to neighbors
			Coord size = transfers.getSize();
			utils::grid<std::vector<Particle>*,3,3,3> neighbors;
			Coord center = pos * 3 + Coord{1,1,1};
			for(int i = 0; i<3; i++) {
				for(int j = 0; j<3; j++) {
					for(int k = 0; k<3; k++) {
						auto cur = center + Coord{i-1,j-1,k-1} * 2;
						if (cur[0] < 0 || cur[0] >= size[0]) continue;
						if (cur[1] < 0 || cur[1] >= size[1]) continue;
						if (cur[2] < 0 || cur[2] >= size[2]) continue;
						neighbors[{i,j,k}] = &transfers[cur];
					}
				}
			}

			// move particles
			std::vector<Particle> remaining;
			remaining.reserve(particles.size());
			for(const auto& p : particles) {

				// compute relative position
				double rx = p.x - x;
				double ry = p.y - y;
				double rz = p.z - z;

				if ((fabs(rx) > dx/2) || (fabs(ry) > dy/2) || (fabs(rz) > dz/2)) {

					// compute corresponding neighbor cell
					int i = (rx < 0) ? 0 : 2;
					int j = (ry < 0) ? 0 : 2;
					int k = (rz < 0) ? 0 : 2;

					// send to neighbor cell
					auto target = neighbors[{i,j,k}];
					if (target) target->push_back(p);

				} else {
					// keep particle
					remaining.push_back(p);
				}
			}

			// update content
			particles.swap(remaining);

		}

		/**
 		 * Initial version of the Field Solver: compute fields E and B for the Boris mover
 		 */
		void computeFields(const Coord& pos, Vector<double> &E, Vector<double> &B){
			// compute forces: E and B
			E.x = sin(2.0 * M_PI * pos[0]) * cos(2.0 * M_PI * pos[1]);
			E.y = pos[0] * (1.0 - pos[0]) * pos[1] * (1.0 - pos[1]);
			E.z = pos[0] * pos[0] + pos[2] * pos[2];
			B.x = 0.0;
			B.y = cos(2.0 * M_PI * pos[2]);
			B.z = sin(2.0 * M_PI * pos[0]);
		}

		/**
 		 * Initialize particles.
 		 * This step is required for the Boris mover
 		 *
		 * @param time step
		 */
		void initParticles(const Coord& pos, const double dt) {

			// quick-check
			if (particles.empty())
				return;

			// update particles
			parec::pfor(particles, [&](Particle& p){
				Vector<double> v, vr;
				double B_sq, f1, f2;
				double qdto4mc = p.q * dt * 0.25;

				Vector<double> E, B;
				computeFields(pos, E, B);

				B_sq = B.x * B.x + B.y * B.y + B.z * B.z;
				f1 = tan(qdto4mc * sqrt(B_sq)) / sqrt(B_sq);
				f2 = 2.0 * f1 / (1.0 + f1 * f1 * B_sq);

				// update velocity
				v.x = p.dx + E.x * qdto4mc;
				v.y = p.dy + E.y * qdto4mc;
				v.z = p.dz + E.z * qdto4mc;

				vr.x = v.x + f1 * (v.y * B.z - B.y * v.z);
				vr.y = v.y + f1 * (-v.x * B.z + v.z * B.x);
				vr.z = v.z + f1 * (v.x * B.y - v.y * B.x);
				v.x = v.x + f2 * (vr.y * B.z - vr.z * B.y);
				v.y = v.y + f2 * (-vr.x * B.z + vr.z * B.x);
				v.z = v.z + f2 * (vr.z * B.y - vr.y * B.x);

				p.vxstar = v.x + E.x * qdto4mc;
				p.vystar = v.y + E.y * qdto4mc;
				p.vzstar = v.z + E.z * qdto4mc;
			});
		}

		/**
 		 * Boris mover in cartesian grid
 		 *
		 * This method is updating the position of all particles within this cell for a single
		 * time step, thereby considering the given field as a driving force. Particles
		 * leaving the cell are submitted via channels to neighboring cells.
		 *
		 * @param pos the coordinates of this cell in the grid
		 * @param field the most recently computed state of the surrounding force fields
		 * @param others a grid of channels providing connections to other cells
		 * @param time step
		 */
		void BorisMover(const Coord& pos, const Field& field, utils::Grid<std::vector<Particle>,3>& transfers, const double dt) {

			// quick-check
			if (particles.empty())
				return;

			// update particles
			parec::pfor(particles, [&](Particle& p){
				Vector<double> v, vr;
				double B_sq, f1, f2;
				double qdto2mc = p.q * dt * 0.5;

				// move particle
				p.x += p.vxstar * dt;
				p.y += p.vystar * dt;
				p.z += p.vzstar * dt;

				Vector<double> E, B;
				computeFields(pos, E, B);

				B_sq = B.x * B.x + B.y * B.y + B.z * B.z;
				f1 = tan(qdto2mc * sqrt(B_sq)) / sqrt(B_sq);
				f2 = 2.0 * f1 / (1.0 + f1 * f1 * B_sq);

				// update velocity
				v.x = p.vxstar + E.x * qdto2mc;
				v.y = p.vystar + E.y * qdto2mc;
				v.z = p.vzstar + E.z * qdto2mc;

				vr.x = v.x + f1 * (v.y * B.z - B.y * v.z);
				vr.y = v.y + f1 * (-v.x * B.z + v.z * B.x);
				vr.z = v.z + f1 * (v.x * B.y - v.y * B.x);
				v.x = v.x + f2 * (vr.y * B.z - vr.z * B.y);
				v.y = v.y + f2 * (-vr.x * B.z + vr.z * B.x);
				v.z = v.z + f2 * (vr.x * B.y - vr.y * B.x);

				vr.x = v.x + E.x * qdto2mc;
				vr.y = v.y + E.y * qdto2mc;
				vr.z = v.z + E.z * qdto2mc;

				p.dx = (p.vxstar + vr.x) * 0.5;
				p.dy = (p.vystar + vr.y) * 0.5;
				p.dz = (p.vzstar + vr.z) * 0.5;

				p.vxstar = vr.x;
				p.vystar = vr.y;
				p.vzstar = vr.z;
			});

			// TODO: test this particles exchanger
			/*// get buffers for particles to be send to neighbors
			Coord size = transfers.getSize();
			utils::grid<std::vector<Particle>*,3,3,3> neighbors;
			Coord center = pos * 3 + Coord{1,1,1};
			for(int i = 0; i<3; i++) {
				for(int j = 0; j<3; j++) {
					for(int k = 0; k<3; k++) {
						auto cur = center + Coord{i-1,j-1,k-1} * 2;
						if (cur[0] < 0 || cur[0] >= size[0]) continue;
						if (cur[1] < 0 || cur[1] >= size[1]) continue;
						if (cur[2] < 0 || cur[2] >= size[2]) continue;
						neighbors[{i,j,k}] = &transfers[cur];
					}
				}
			}

			// move particles
			std::vector<Particle> remaining;
			remaining.reserve(particles.size());
			for(const auto& p : particles) {

				// compute relative position
				double rx = p.x - x;
				double ry = p.y - y;
				double rz = p.z - z;

				if ((fabs(rx) > dx/2) || (fabs(ry) > dy/2) || (fabs(rz) > dz/2)) {

					// compute corresponding neighbor cell
					int i = (rx < 0) ? 0 : 2;
					int j = (ry < 0) ? 0 : 2;
					int k = (rz < 0) ? 0 : 2;

					// send to neighbor cell
					auto target = neighbors[{i,j,k}];
					if (target) target->push_back(p);

				} else {
					// keep particle
					remaining.push_back(p);
				}
			}

			// update content
			particles.swap(remaining);*/
		}

		/**
 		 * Interpolation of fields to particles and the Boris mover in cartesian grid
 		 *
		 * This method is updating the position of all particles within this cell for a single
		 * time step, thereby considering the given field as a driving force. Particles
		 * leaving the cell are submitted via channels to neighboring cells.
		 *
		 * @param pos the coordinates of this cell in the grid
		 * @param field the most recently computed state of the surrounding force fields
		 * @param others a grid of channels providing connections to other cells
		 * @param time step
		 */
		void InterF2PBorisMover(const Coord& pos, const Field& field, utils::Grid<std::vector<Particle>,3>& transfers, const double dt) {

			// quick-check
			if (particles.empty())
				return;

			// extract forces
			Vector<double> E[2][2][2];
			Vector<double> B[2][2][2];
			for(int i=0; i<2; i++) {
				for(int j=0; j<2; j++) {
					for(int k=0; k<2; k++) {
						Coord cur({pos[0]+i,pos[1]+j,pos[2]+k});
						E[i][j][k] = field[cur].E;
						B[i][j][k] = field[cur].B;
					}
				}
			}

			// update particles
			parec::pfor(particles, [&](Particle& p){
				Vector<double> v, vr;
				double B_sq, f1, f2;
				const double qdto2mc = p.q * dt * 0.5;

				// BEGIN of interpolation of fields to particles
				Vector<double> corig, vorig, cavg, vavg;
				corig.x = p.x;
				corig.y = p.y;
				corig.z = p.z;
				vorig.x = p.dx;
				vorig.y = p.dy;
				vorig.z = p.dz;
				cavg = corig;
				vavg = vorig;

				// END of interpolation of fields to particles

				// move particle
				p.x += p.vxstar * dt;
				p.y += p.vystar * dt;
				p.z += p.vzstar * dt;

				Vector<double> E, B;
				computeFields(pos, E, B);

				B_sq = B.x * B.x + B.y * B.y + B.z * B.z;
				f1 = tan(qdto2mc * sqrt(B_sq)) / sqrt(B_sq);
				f2 = 2.0 * f1 / (1.0 + f1 * f1 * B_sq);

				// update velocity
				v.x = p.vxstar + E.x * qdto2mc;
				v.y = p.vystar + E.y * qdto2mc;
				v.z = p.vzstar + E.z * qdto2mc;

				vr.x = v.x + f1 * (v.y * B.z - B.y * v.z);
				vr.y = v.y + f1 * (-v.x * B.z + v.z * B.x);
				vr.z = v.z + f1 * (v.x * B.y - v.y * B.x);
				v.x = v.x + f2 * (vr.y * B.z - vr.z * B.y);
				v.y = v.y + f2 * (-vr.x * B.z + vr.z * B.x);
				v.z = v.z + f2 * (vr.x * B.y - vr.y * B.x);

				vr.x = v.x + E.x * qdto2mc;
				vr.y = v.y + E.y * qdto2mc;
				vr.z = v.z + E.z * qdto2mc;

				p.dx = (p.vxstar + vr.x) * 0.5;
				p.dy = (p.vystar + vr.y) * 0.5;
				p.dz = (p.vzstar + vr.z) * 0.5;

				p.vxstar = vr.x;
				p.vystar = vr.y;
				p.vzstar = vr.z;
			});

			// TODO: test this particles exchanger
			/*// get buffers for particles to be send to neighbors
			Coord size = transfers.getSize();
			utils::grid<std::vector<Particle>*,3,3,3> neighbors;
			Coord center = pos * 3 + Coord{1,1,1};
			for(int i = 0; i<3; i++) {
				for(int j = 0; j<3; j++) {
					for(int k = 0; k<3; k++) {
						auto cur = center + Coord{i-1,j-1,k-1} * 2;
						if (cur[0] < 0 || cur[0] >= size[0]) continue;
						if (cur[1] < 0 || cur[1] >= size[1]) continue;
						if (cur[2] < 0 || cur[2] >= size[2]) continue;
						neighbors[{i,j,k}] = &transfers[cur];
					}
				}
			}

			// move particles
			std::vector<Particle> remaining;
			remaining.reserve(particles.size());
			for(const auto& p : particles) {

				// compute relative position
				double rx = p.x - x;
				double ry = p.y - y;
				double rz = p.z - z;

				if ((fabs(rx) > dx/2) || (fabs(ry) > dy/2) || (fabs(rz) > dz/2)) {

					// compute corresponding neighbor cell
					int i = (rx < 0) ? 0 : 2;
					int j = (ry < 0) ? 0 : 2;
					int k = (rz < 0) ? 0 : 2;

					// send to neighbor cell
					auto target = neighbors[{i,j,k}];
					if (target) target->push_back(p);

				} else {
					// keep particle
					remaining.push_back(p);
				}
			}

			// update content
			particles.swap(remaining);*/
		}

		/**
		 * Requests this cell to import all the particles from the given source (receiving end of a channel).
		 */
		void importParticles(const Coord& pos, utils::Grid<std::vector<Particle>,3>& transfers) {

			// import particles send to this cell
			Coord size = transfers.getSize();
			Coord center = pos * 3 + Coord{1,1,1};
			for(int i = 0; i<3; i++) {
				for(int j = 0; j<3; j++) {
					for(int k = 0; k<3; k++) {
						auto cur = center + Coord{i-1,j-1,k-1};
						if (cur[0] < 0 || cur[0] >= size[0]) continue;
						if (cur[1] < 0 || cur[1] >= size[1]) continue;
						if (cur[2] < 0 || cur[2] >= size[2]) continue;

						auto& in = transfers[cur];
						particles.insert(particles.end(), in.begin(), in.end());
						in.clear();
					}
				}
			}

		}

	};

	/*
 	 * Field solver -- solving Maxwell's equation
 	 */
	void FieldSolver(){
	}


	void solveMaxwellEquations(utils::Grid<FieldCell,3>& res, const utils::Grid<DensityCell,3>& density) {

		// TODO: solve Maxwell's Equation by solving the linear equation
		//
		//   					A x = b
		//
		// where x ist the field matrix and b is c1 * density.rho + c2 * density.J

//		Coord zero({0,0,0});
//		Coord size = res.getSize();
//
//		utils::Grid<FieldCell,1> x;
//		utils::Grid<FieldCell,1> b;
//
//		parec::pfor(zero,size,[&](const Coord& pos) {
//
//		});
//
//		double residual_norm = std::limit<double>::max();
//		while(residual_norm > 1) {
//			residual_norm -= 1.0;	// just for testing
//		}

		// set all values of E to 0 for now
		utils::pfor(res.getSize(),[&](const utils::Coordinate<3>& cur) {
			res[cur].E.x = 0.0;
			res[cur].E.y = 0.0;
			res[cur].E.z = 0.0;
		});


	}


	void MaxwellEquations_Test() {
		int N = 10;
		utils::Size<3> zero({0,0,0});
		utils::Size<3> size({N,N,N});
		utils::Grid<DensityCell,3> density(size);
		utils::Grid<FieldCell,3> field(size);

		// initialize
		utils::pfor(zero,size,[&](const utils::Coordinate<3>& cur) {
			density[cur].rho = 0.0;
			density[cur].J = 0.0;
		});

		// solve equations
		solveMaxwellEquations(field,density);

		// TODO: build an actual test case
	}

	/**
	 * The following test case represents the actual iPic3D prototype implementation. At its core,
	 * it utilizes a 3D grid of cells, each cell containing a list of particles. The cell structure
	 * is defined by the Cell class above. Furthermore, a density and magnetic/electric field grid
	 * is utilized.
	 *
	 * To facilitate communication between entities, e.g. to transfer particles between cells or
	 * aggregate the effect of particles on field cells, channels are utilized. Channels are
	 * synchronization entities relying typed information between threads. For instance, a
	 * instance of
	 *
	 * 									Channel<Particle>
	 *
	 * allows users to transfer Particle instances between concurrent threads in a synchronized way.
	 * Let c be an instance of Channel<Particle>. To submit a particle p, once can utilize
	 *
	 * 								  c.getSink().send(p);
	 *
	 * In this case a sink (sending end) of channel c is obtained, the particle p is submitted, and
	 * and all buffers of the sink are flushed (automatically by the destructor of the Sink). To
	 * submit multiple particles, once can use
	 *
	 * 							      auto snk = c.getSink();
	 * 							      for( ... ) {
	 * 							      		snk.send(...);
	 * 							      }
	 * 							      snk.flush();
	 *
	 * or simply relay on the destructor using
	 *
	 * 							      {
	 * 							          auto snk = c.getSink();
	 * 							          for( ... ) {
	 * 							          		snk.send(...);
	 * 							          }
	 * 							      }
	 *
	 * Similar, to receive data transfered through a channel c, one can use
	 *
	 * 							      c.getSource().receive();
	 *
	 * which will yield a particle from the channel or block if there is no such particle. To test
	 * whether there is a particle available,
	 *
	 * 								  c.getSource().empty();
	 *
	 * can be queried.
	 */

	void iPic3D_Solver() {

		// In this demo we create a grid of cells and insert a single particle in cell [0,0,0], moving
		// in the direction [1,1,1] at a speed such that it will pass one cell in a time step.
		// The test case is observing the trace of the particle throughout the simulation.


		// some simulation parameters
		int N = 10;
		double dx = 10;
		double dy = 10;
		double dz = 10;


		// ----- setup -----

		utils::Size<3> size = N;							// the size of the grid

		const utils::Coordinate<3> zero = 0;				// a zero constant (coordinate [0,0,0])
		const utils::Coordinate<3> full = size;				// a constant covering the full range

		// the 3-D grid of cells
		utils::Grid<Cell,3> cells(size);									// the grid of cells containing the particles
		utils::Grid<std::vector<Particle>,3> particleTransferes(size * 3);	// a grid of buffers for transferring particles between cells

		// initialize all cells
		utils::pfor(zero, full, [&](const utils::Coordinate<3>& pos) {

			Cell& cell = cells[pos];

			// physical properties
			cell.x = pos[0] * dx;
			cell.dx = dx;

			cell.y = pos[1] * dy;
			cell.dy = dy;

			cell.z = pos[2] * dz;
			cell.dz = dz;

		});

		// the field size
		utils::Size<3> fieldSize = {N+1,N+1,N+1};

		// the 3-D force field
		Field field(fieldSize);

		// the 3-D density field
		Density density(fieldSize);

		// a 3-D structure collecting contributions of cells to the density
		utils::Grid<DensityCell,3> densityContributions(fieldSize*2);

		// initialize contributions to 0
		utils::pfor(fieldSize*2,[&](const utils::Coordinate<3>& pos){
			auto& entry = densityContributions[pos];
			entry.rho = 0.0;
			entry.J = 0.0;
		});

		// insert a particle into first cell
		Particle p{ 0, 0, 0, dx, dy, dz };							// particle moving diagonal though space
		cells[{0,0,0}].particles.push_back(p);
		std::cout << (1 == (cells[{0,0,0}].particles.size())) << std::endl;
		std::cout << (0 == (cells[{N-1,N-1,N-1}].particles.size())) << std::endl;


		// create the output file
		auto& manager = utils::io::FileIOManager::getInstance();

		// create the result file
		auto logFile = manager.createEntry("particle_log.txt", utils::io::Mode::Text);
		manager.openOutputStream(logFile) << "time,x,y,z,num_particles\n";

		auto blogFile = manager.createEntry("particle_log.bin", utils::io::Mode::Binary);
		manager.openOutputStream(blogFile);

		// ----- simulation -----


		// propagate particles
		for(int i=0; i<N-1; i++) {
			std::cout << (1 == (cells[{i,i,i}].particles.size())) << std::endl;

			// project particles to density field
			utils::pfor(zero,full,[&](const utils::Coordinate<3>& pos) {
				cells[pos].projectToDensityField(pos, densityContributions);
			});

			// update density field
			utils::pfor(zero,fieldSize,[&](const utils::Coordinate<3>& pos) {
				DensityCell& entry = density[pos];

				// reset density field
				entry.rho = 0.0;
				entry.J = 0.0;

				// aggregate contributions of adjacent cell
				for(int i=0; i<2; i++) {
					for(int j=0; j<2; j++) {
						for(int k=0; k<2; k++) {
							auto& cur = densityContributions[(pos * 2) + utils::Coordinate<3>{i,j,k}];
							entry.rho += cur.rho;
							entry.J += cur.J;
						}
					}
				}
			});

			// propagate field forces according to Maxwell's equations (GMRES)
			solveMaxwellEquations(field, density);

			// calculate Faraday's law
			utils::pfor(zero,fieldSize,[&](const utils::Coordinate<3>& pos) {
				field[pos].B.x = 0.0; // calculateB(field[pos].E, field[pos+{0,0,1}].E, density[pos]);
				field[pos].B.y = 0.0; // calculateB(field[pos].E, field[pos+{0,0,1}].E, density[pos]);
				field[pos].B.z = 0.0; // calculateB(field[pos].E, field[pos+{0,0,1}].E, density[pos]);
			});

			// move particles
			utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){
				cells[pos].moveParticles(pos,field,particleTransferes);
			});
			// -- implicit global sync - TODO: can this be eliminated? --

			utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){
				cells[pos].importParticles(pos,particleTransferes);
			});
			// -- implicit global sync - TODO: can this be eliminated? --

			std::cout << (1 == (cells[{i+1,i+1,i+1}].particles.size())) << std::endl;

			// write particle statistics
			if ((i % 3) == 0) {		// - every 10th timestep
				utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){

					auto num = cells[pos].particles.size();

					// print a text log
					manager.getOutputStream(logFile).atomic([&](auto& out){
						// print number of particles in cell
						out << i << "," << pos[0] << "," << pos[1] << "," << pos[2] << "," << num << "\n";
					});

					// print a binary log
					manager.getOutputStream(blogFile).atomic([&](auto& out){
						// print number of particles in cell
						out .template write<short>(pos[0])
							.template write<short>(pos[1])
							.template write<short>(pos[2])
							.write(num);
					});
				});
			}

		}

		std::cout << (0 == (cells[{0,0,0}].particles.size())) << std::endl;
		std::cout << (1 == (cells[{N-1,N-1,N-1}].particles.size())) << std::endl;

	}

	/**
 	 * Our test for Boris mover
 	 * 	One cell and one particle
 	 */
	void iPic3D_BorisMover() {
		// In this demo we create a grid of cells and insert a single particle in cell [0,0,0], moving
		// in the direction [1,1,1] at a speed such that it will pass one cell in a time step.
		// The test case is observing the trace of the particle throughout the simulation.

		// some simulation parameters
		int N = 1;
		double dx = 10;
		double dy = 10;
		double dz = 10;


		// ----- setup -----
		utils::Size<3> size = N;							// the size of the grid

		const utils::Coordinate<3> zero = 0;				// a zero constant (coordinate [0,0,0])
		const utils::Coordinate<3> full = size;				// a constant covering the full range

		// the 3-D grid of cells
		utils::Grid<Cell,3> cells(size);									// the grid of cells containing the particles
		utils::Grid<std::vector<Particle>,3> particleTransferes(size * 3);	// a grid of buffers for transferring particles between cells

		// initialize all cells
		// TODO: revise
		utils::pfor(zero, full, [&](const utils::Coordinate<3>& pos) {

			Cell& cell = cells[pos];

			// physical properties
			cell.x = pos[0] * dx;
			cell.dx = dx;

			cell.y = pos[1] * dy;
			cell.dy = dy;

			cell.z = pos[2] * dz;
			cell.dz = dz;

		});


		// the field size
		utils::Size<3> fieldSize = {N+1,N+1,N+1};

		// the 3-D force field
		Field field(fieldSize);

		// the 3-D density field
		Density density(fieldSize);

		// a 3-D structure collecting contributions of cells to the density
		utils::Grid<DensityCell,3> densityContributions(fieldSize*2);

		// initialize contributions to 0
		utils::pfor(fieldSize*2,[&](const utils::Coordinate<3>& pos){
			auto& entry = densityContributions[pos];
			entry.rho = 0.0;
			entry.J = 0.0;
		});

		// insert a particle into the first cell
		Particle p{ 0.35, 0.25, 0.0, 0.25, 0.25, 0.25, -1.0 };
		cells[{0,0,0}].particles.push_back(p);
		std::cout << (1 == (cells[{0,0,0}].particles.size())) << std::endl;

		// create the output file
		auto& manager = utils::io::FileIOManager::getInstance();

		// create the result file
		// text file
		auto logFile = manager.createEntry("ipic3d_particles_log.txt", utils::io::Mode::Text);
		manager.openOutputStream(logFile) << "x,y,z,vx,vy,vz\n";
		// binary file
		//auto blogFile = manager.createEntry("ipic3d_particles_log.bin", utils::io::Mode::Binary);
		//manager.openOutputStream(blogFile);


		// ----- simulation ----i-
		// initialize all particles
		double dt = 0.00625, tcur = 0.0, tend = 4.0;
		utils::pfor(zero, size, [&](const utils::Coordinate<3>& pos) {
			cells[pos].initParticles(pos,dt);
		});

		// propagate particles
		int iter = 0;
		while( tcur <= tend ) {

			// interpolation of particles to grid: project particles to the density field
			utils::pfor(zero,full,[&](const utils::Coordinate<3>& pos) {
				cells[pos].interP2G(pos, densityContributions);
			});

			// update density field
			utils::pfor(zero,fieldSize,[&](const utils::Coordinate<3>& pos) {
				DensityCell& entry = density[pos];

				// reset density field
				entry.rho = 0.0;
				entry.J = 0.0;

				// aggregate contributions of adjacent cell
				for(int i=0; i<2; i++) {
					for(int j=0; j<2; j++) {
						for(int k=0; k<2; k++) {
							auto& cur = densityContributions[(pos * 2) + utils::Coordinate<3>{i,j,k}];
							entry.rho += cur.rho;
							entry.J += cur.J;
						}
					}
				}
			});


			// propagate field forces according to Maxwell's equations (GMRES)
			FieldSolver();


			// move particles
			utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){
				cells[pos].BorisMover(pos, field, particleTransferes, dt);
			});
			// -- implicit global sync - TODO: can this be eliminated? --


			utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){
				cells[pos].importParticles(pos,particleTransferes);
			});
			// -- implicit global sync - TODO: can this be eliminated? --


			// write particle statistics
			if ((iter % 1) == 0) {		// - every nth timestep
				utils::pfor(zero,size,[&](const utils::Coordinate<3>& pos){
					parec::pfor(cells[pos].particles, [&](Particle& p){
						// print a text log
						manager.getOutputStream(logFile).atomic([&](auto& out){
							out << p.x << " " << p.y << " " << p.z << " " << p.dx << " " << p.dy << " " << p.dz << "\n";
						});

						/*// print a binary log
						manager.getOutputStream(blogFile).atomic([&](auto& out){
							out .template write<float>(p.x)
								.template write<float>(p.y)
								.template write<float>(p.z)
								.template write<float>(p.dx)
								.template write<float>(p.dy)
								.template write<float>(p.dz);
						});*/
					});
				});
			}


			// increment time step
			tcur += dt;
			iter++;
		}

		std::cout << (1 == (cells[{0,0,0}].particles.size())) << std::endl;
	}

int main() {
	MaxwellEquations_Test();
	iPic3D_Solver();
	iPic3D_BorisMover();
	return 0;
}
