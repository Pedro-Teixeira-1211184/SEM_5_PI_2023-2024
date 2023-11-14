import { Repo } from "../../core/infra/Repo";
import { Room } from "../../domain/room";

export default interface IRoomRepo extends Repo<Room> {
    save(room: Room): Promise<Room>;
    findByDomainId(roomId: string): Promise<Room>;
    findByName(roomName: string): Promise<Room>;
    delete(room: Room): Promise<void>;
    exists(room: Room): Promise<boolean>;
}
