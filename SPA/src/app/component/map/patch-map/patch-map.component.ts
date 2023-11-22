import {Component, inject, OnInit} from '@angular/core';
import {FloorService} from "../../../services/floor/floor.service";
import {BuildingService} from "../../../services/building/building.service";
import {MapService} from "../../../services/map/map.service";
import {FormArray, FormBuilder, FormControl, FormGroup, Validators} from "@angular/forms";
import {PassagewayService} from "../../../services/passageway/passageway.service";
import IFloorDTO from "../../../dto/IFloorDTO";
import IBuildingDTO from "../../../dto/IBuildingDTO";
import IPassagewayDTO from "../../../dto/IPassagewayDTO";
import {ElevatorService} from "../../../services/elevator/elevator.service";
import IElevatorDTO from "../../../dto/IElevatorDTO";
import {RoomService} from "../../../services/room/room.service";
import IRoomDTO from "../../../dto/IRoomDTO";
import IMapRoomDTO from "../../../dto/IMapRoomDTO";
import IMapPassagewayDTO from "../../../dto/IMapPassagewayDTO";
import IMapElevatorDTO from "../../../dto/IMapElevatorDTO";

@Component({
  selector: 'app-patch-map',
  templateUrl: './patch-map.component.html',
  styleUrls: ['./patch-map.component.css']
})
export class PatchMapComponent implements OnInit {

  service = inject(MapService);
  b_service = inject(BuildingService);
  p_service = inject(PassagewayService);
  f_service = inject(FloorService);
  e_service = inject(ElevatorService);
  r_service = inject(RoomService);

  mapForm!: FormGroup;
  form!: FormGroup;
  roomForm!: FormGroup;
  passagewayForm!: FormGroup;
  elevatorsForm!: FormGroup;

  buildings: IBuildingDTO[] = [];
  floors: IFloorDTO[] = [];
  passageways: IPassagewayDTO[] = [];
  elevators: IElevatorDTO[] = [];
  rooms: IRoomDTO[] = [];

  hasRooms: boolean = false;
  hasMap: boolean = false;
  hasPassageways: boolean = false;
  hasElevators: boolean = false;

  columns: number = 0;
  rows: number = 0;
  floorNumber = 0;

  constructor(private fb: FormBuilder) {
    this.getBuildingsCodes();
  }

  ngOnInit(): void {
    this.mapForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorCode: new FormControl('', [Validators.required]),
    });
    // Defina a matriz e os controles do formulário ao inicializar o componente
    this.form = this.fb.group({
      matrix: this.fb.array([])
    });

    this.roomForm = this.fb.group({
      roomMatrix: this.fb.array([])
    });

    this.passagewayForm = this.fb.group({
      passagewayMatrix: this.fb.array([])
    });

    this.elevatorsForm = this.fb.group({
      elevatorMatrix: this.fb.array([])
    });
  }

  initializeMatrix() {
    //reset matrix
    this.form = this.fb.group({
      matrix: this.fb.array([])
    });
    // Neste exemplo, criaremos uma matriz 3x3
    for (let i = 0; i < this.columns; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < this.rows; j++) {
        row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
      }
      this.matrix.push(row);
    }
  }

  initializeMapMatrix() {
    //reset matrix
    this.roomForm = this.fb.group({
      roomMatrix: this.fb.array([])
    });

    for (let i = 0; i < this.rooms.length; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < 9; j++) {
        if (j == 0) {
          row.push(this.fb.control(this.rooms[i].designation, [Validators.required]));
        }
        if (j == 1) {
          row.push(this.fb.control(this.rooms[i].name, [Validators.required]));
        }
        if (j == 2) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 3) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 4) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 5) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 6) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 7) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 8) {
          row.push(this.fb.control(0, [Validators.required]));
        }
      }
      this.roomMatrix.push(row);
    }
  }

  initializePassagewayMatrix() {
    //reset matrix
    this.passagewayForm = this.fb.group({
      passagewayMatrix: this.fb.array([])
    });

    for (let i = 0; i < this.passageways.length; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < 5; j++) {
        if (j == 0) {
          row.push(this.fb.control(this.passageways[i].floorCode1, [Validators.required]));
        }
        if (j == 1) {
          row.push(this.fb.control(this.passageways[i].floorCode2, [Validators.required]));
        }
        if (j == 2) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 3) {
          row.push(this.fb.control(0, [Validators.min(0), Validators.max(3)]));
        }
        if (j == 4) {
          row.push(this.fb.control(0, [Validators.required]));
        }
      }
      this.passagewayMatrix.push(row);
    }
  }

  initializeElevatorMatrix() {
    //reset matrix
    this.elevatorsForm = this.fb.group({
      elevatorMatrix: this.fb.array([])
    });

    for (let i = 0; i < this.elevators.length; i++) {
      const row = this.fb.array([]);
      for (let j = 0; j < 4; j++) {
        if (j == 0) {
          row.push(this.fb.control(this.elevators[i].floorNumbers, [Validators.required]));
        }
        if (j == 1) {
          row.push(this.fb.control(0, [Validators.required]));
        }
        if (j == 2) {
          row.push(this.fb.control(0, [Validators.required]));
        }
        if (j == 3) {
          row.push(this.fb.control(0, [Validators.required]));
        }
      }
      this.elevatorMatrix.push(row);
    }
  }

  private async getBuildingsCodes() {
    this.buildings = await this.b_service.getAllBuildings();
  }

  private async getFloorsCodes(buildingCode: string) {
    this.floors = [];
    this.floors = await this.f_service.getFloorsByBuildingCode(buildingCode);
  }

  get buildingCode() {
    return this.mapForm.get('buildingCode');
  }

  get floorCode() {
    return this.mapForm.get('floorCode');
  }

  get matrix() {
    return this.form.get('matrix') as FormArray;
  }

  get roomMatrix() {
    return this.roomForm.get('roomMatrix') as FormArray;
  }

  get passagewayMatrix() {
    return this.passagewayForm.get('passagewayMatrix') as FormArray;
  }

  get elevatorMatrix() {
    return this.elevatorsForm.get('elevatorMatrix') as FormArray;
  }

  public async getFloorsOfBuilding() {
    await this.clean();
    //set grid dimensions
    for (let i = 0; i < this.buildings.length; i++) {
      if (this.buildings[i].code == this.buildingCode?.value) {
        this.columns = this.buildings[i].dimensions.length + 1;
        this.rows = this.buildings[i].dimensions.width + 1;
        break;
      }
    }
    this.hasMap = true;
    this.initializeMatrix();
    await this.getFloorsCodes(this.buildingCode?.value);
  }

  // Função para obter o controlo de uma célula específica
  getCellControl(rowIndex: number, colIndex: number) {
    return (this.form.get('matrix') as FormGroup).get(`${rowIndex}.${colIndex}`) as FormControl;
  }

  // Função chamada quando o valor da célula é alterado
  onCellValueChange(rowIndex: number, colIndex: number) {
    try {
      const cellControl = this.getCellControl(rowIndex, colIndex);
      console.log(`Valor alterado na célula (${rowIndex}, ${colIndex}): ${cellControl?.value}`);
    } catch (e) {
    }
  }

  getCellControlRoom(rowIndex: number, colIndex: number) {
    return (this.roomForm.get('roomMatrix') as FormGroup).get(`${rowIndex}.${colIndex}`) as FormControl;
  }

  // Função chamada quando o valor da célula é alterado
  onCellValueChangeRoom(rowIndex: number, colIndex: number) {
    try {
      const cellControl = this.getCellControlRoom(rowIndex, colIndex);
      console.log(`Valor alterado na célula (${rowIndex}, ${colIndex}): ${cellControl?.value}`);
    } catch (e) {
    }
  }

  getCellControlPassageway(rowIndex: number, colIndex: number) {
    return (this.passagewayForm.get('passagewayMatrix') as FormGroup).get(`${rowIndex}.${colIndex}`) as FormControl;
  }

  onCellValueChangePassageway(rowIndex: number, colIndex: number) {
    try {
      const cellControl = this.getCellControlPassageway(rowIndex, colIndex);
      console.log(`Valor alterado na célula (${rowIndex}, ${colIndex}): ${cellControl?.value}`);
    } catch (e) {
    }
  }


  getCellControlElevator(rowIndex: number, colIndex: number) {
    return (this.elevatorsForm.get('elevatorMatrix') as FormGroup).get(`${rowIndex}.${colIndex}`) as FormControl;
  }

  onCellValueChangeElevator(rowIndex: number, colIndex: number) {
    try {
      const cellControl = this.getCellControlElevator(rowIndex, colIndex);
      console.log(`Valor alterado na célula (${rowIndex}, ${colIndex}): ${cellControl?.value}`);
    } catch (e) {
    }
  }

  public async getFloorElements() {
    await this.clean();
    this.floorNumber = 0;
    for (let i = 0; i < this.floors.length; i++) {
      if (this.floors[i].code == this.floorCode?.value) {
        this.floorNumber = this.floors[i].number;
        break;
      }
    }

    //get passageways
    this.passageways = await this.p_service.getPassageWayByFloorCode(this.floorCode?.value);

    const initialElevators = await this.e_service.getElevatorsByBuildingCode(this.buildingCode?.value);
    if (initialElevators.length > 0) {
      for (let i = 0; i < initialElevators.length; i++) {
        const floorNumbers = initialElevators[i].floorNumbers.split(',');
        for (let j = 0; j < floorNumbers.length; j++) {
          if (Number(floorNumbers[j]) == this.floorNumber) {
            this.elevators.push(initialElevators[i]);
            break;
          }
        }
      }
      if (this.elevators.length == 0) {
        alert("There are no elevators in this floor");
      }
    }

    this.rooms = await this.r_service.getRoomsByFloorCode(this.floorCode?.value);
    this.hasRooms = this.rooms.length > 0;
    if (this.hasRooms) {
      this.initializeMapMatrix();
    }
    this.hasPassageways = this.passageways.length > 0;
    if (this.hasPassageways) {
      this.initializePassagewayMatrix();
    }
    this.hasElevators = this.elevators.length > 0;
    if (this.hasElevators) {
      this.initializeElevatorMatrix();
    }
  }

  private async clean() {
    this.hasRooms = false;
    this.hasPassageways = false;
    this.hasElevators = false;
    this.passageways = [];
    this.elevators = [];
    this.rooms = [];
  }

  public async submit() {
    //TODO: submit
    let rooms: IMapRoomDTO[] = [];
    for (let i = 0; i < this.roomMatrix.length; i++) {
      const room: IMapRoomDTO = {
        name: this.roomMatrix.controls[i].value[1],
        dimensions: {
          top: {
            x: this.roomMatrix.controls[i].value[4],
            y: this.roomMatrix.controls[i].value[5]
          },
          bottom: {
            x: this.roomMatrix.controls[i].value[6],
            y: this.roomMatrix.controls[i].value[7]
          }
        },
        door: {
          coordinates: {
            x: this.roomMatrix.controls[i].value[2],
            y: this.roomMatrix.controls[i].value[3]
          },
          orientation: this.roomMatrix.controls[i].value[8]
        }
      }
      rooms.push(room);
    }
    let passageways: IMapPassagewayDTO[] = [];
    for (let i = 0; i < this.passagewayMatrix.length; i++) {
      const passageway: IMapPassagewayDTO = {
        start: this.passagewayMatrix.controls[i].value[0],
        end: this.passagewayMatrix.controls[i].value[1],
        localization: {
          coordinates: {
            x: this.passagewayMatrix.controls[i].value[2],
            y: this.passagewayMatrix.controls[i].value[3]
          },
          orientation: this.passagewayMatrix.controls[i].value[4]
        }
      }
      passageways.push(passageway);
    }
    let elevators: IMapElevatorDTO[] = [];
    for (let i = 0; i < this.elevatorMatrix.length; i++) {
      const elevator: IMapElevatorDTO = {
        localization: {
          coordinates: {
            x: this.elevatorMatrix.controls[i].value[1],
            y: this.elevatorMatrix.controls[i].value[2]
          },
          orientation: this.elevatorMatrix.controls[i].value[3]
        }
      }
      elevators.push(elevator);
    }
    console.log(elevators)
    await this.service.patchMap(this.buildingCode?.value, this.floorNumber, this.columns, this.rows, this.matrix.value, rooms, passageways, elevators);
  }
}

// Path: SPA/src/app/component/map/patch-map/patch-map.component.html
