import {Component, inject, OnInit} from '@angular/core';
import {RoomService} from "../../../services/room/room.service";
import {BuildingService} from "../../../services/building/building.service";
import {FloorService} from "../../../services/floor/floor.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-create-room',
  templateUrl: './create-room.component.html',
  styleUrls: ['./create-room.component.css']
})
export class CreateRoomComponent implements OnInit {

  service: RoomService = inject(RoomService);
  b_service: BuildingService = inject(BuildingService);
  f_service: FloorService = inject(FloorService);

  buildingsCodes: string[] = [];
  floorsCode: string[] = [];

  roomForm!: FormGroup;

  ngOnInit(): void {
    this.roomForm = new FormGroup({
      floorCode: new FormControl('', [Validators.required]),
      designation: new FormControl('', [Validators.required]),
      name: new FormControl('', [Validators.required]),
    });
  }

  constructor() {
    this.getBuildingsCodes();
  }

  private async getBuildingsCodes() {
    this.buildingsCodes = await this.b_service.getAllBuildingsCode();
    for (let buildingCode of this.buildingsCodes) {
      const x = await this.f_service.getFloorsByBuildingCode(buildingCode);
      for (let floor of x) {
        this.floorsCode.push(floor.code);
      }
    }
  }

  public async submit() {
    if (this.roomForm.valid) {
      await this.service.createRoom(this.floorCode?.value, this.designation?.value, this.name?.value);
    }
  }

  get floorCode() {
    return this.roomForm.get('floorCode');
  }

  get designation() {
    return this.roomForm.get('designation');
  }

  get name() {
    return this.roomForm.get('name');
  }

}