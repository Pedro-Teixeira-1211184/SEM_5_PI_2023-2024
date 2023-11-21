import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";

import IPathResultDTO from "../dto/IPathResultDTO";

interface PathResultProps {
    pathBetweenFloors: string[];
    pathPerFloor: number[];
}

export class PathResult extends AggregateRoot<PathResultProps> {
    
        get id (): UniqueEntityID {
            return this._id;
        }
    
        get pathBetweenFloors (): string[] {
            return this.props.pathBetweenFloors;
        }
    
        get pathPerFloor (): number[] {
            return this.props.pathPerFloor;
        }
    
        private constructor (props: PathResultProps, id?: UniqueEntityID) {
            super(props, id);
        }
    
        public static create (pathResultDTO: IPathResultDTO, id?: UniqueEntityID): Result<PathResult> {
            const pathBetweenFloors = pathResultDTO.pathBetweenFloors;
            const pathPerFloor = pathResultDTO.pathPerFloor;

            const pathResult = new PathResult({ pathBetweenFloors, pathPerFloor }, id);
            return Result.ok<PathResult>(pathResult);
        }
    }