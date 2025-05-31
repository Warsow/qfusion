#ifndef WSW_3da69c3d_20cd_4493_a659_fa10040b6b96_H
#define WSW_3da69c3d_20cd_4493_a659_fa10040b6b96_H

/**
 * An implementation of a queue that uses an underlying array
 * of a limited capacity that acts as a limit of operations number as well.
 * This is a very fast queue implementation as doing pointer adjustments/writes is all that it performs.
 */
template <unsigned Limit = 32>
class ArrayBasedFringe {
	int fringe[Limit];
	unsigned head { 0 };
	unsigned tail { 0 };
public:
	bool IsEmpty() const { return head >= tail; }

	bool Add( int areaNum ) {
		if( tail < Limit ) {
			fringe[tail++] = areaNum;
			return true;
		}
		return false;
	}

	int Peek() const {
		assert( !IsEmpty() );
		return fringe[head];
	}

	int Pop() {
		assert( !IsEmpty() );
		return fringe[head++];
	}
};

template <typename Queue>
class SharedFaceAreasWalker {
protected:
	Queue queue;
	BitVector *const __restrict visitedAreas;
	BitVector *const __restrict visitedFaces;
	const AiAasWorld *const __restrict aasWorld;
	const std::span<const aas_area_t> aasAreas;
	const std::span<const aas_face_t> aasFaces;
	const std::span<const aas_faceindex_t> aasFaceIndex;

	virtual bool ProcessAreaTransition( int currArea, int nextArea, const aas_face_t *face ) = 0;
public:
	SharedFaceAreasWalker( int startAreaNum, BitVector *visitedAreas_, BitVector *visitedFaces_ )
		: visitedAreas( visitedAreas_ )
		, visitedFaces( visitedFaces_ )
		, aasWorld( AiAasWorld::instance() )
		, aasAreas( aasWorld->getAreas() )
		, aasFaces( aasWorld->getFaces() )
		, aasFaceIndex( aasWorld->getFaceIndex() ) {
		visitedAreas->Clear();
		visitedFaces->Clear();
		queue.Add( startAreaNum );
	}

	virtual ~SharedFaceAreasWalker() = default;

	virtual void Exec() {
		while( !queue.IsEmpty() ) {
			const int areaNum = queue.Pop();
			visitedAreas->Set( areaNum, true );

			const auto &__restrict area = aasAreas[areaNum];
			for( int faceIndexNum = area.firstface; faceIndexNum < area.firstface + area.numfaces; ++faceIndexNum ) {
				const int faceIndex = aasFaceIndex[faceIndexNum];

				// If the face has been already processed, skip it.
				if( !visitedFaces->TrySet( std::abs( faceIndex ) ) ) {
					continue;
				}

				// Get a actual face and an area behind it by a sign of the faceIndex
				// TODO: AAS data layout should be reorganized and a branchless selection should be used
				const aas_face_t *face;
				int nextAreaNum;
				if( faceIndex >= 0 ) {
					face = &aasFaces[+faceIndex];
					nextAreaNum = face->backarea;
				} else {
					face = &aasFaces[-faceIndex];
					nextAreaNum = face->frontarea;
				}

				if( !ProcessAreaTransition( areaNum, nextAreaNum, face ) ) {
					return;
				}
			}
		}
	}
};

#endif
