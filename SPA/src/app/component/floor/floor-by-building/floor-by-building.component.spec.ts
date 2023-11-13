import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FloorByBuildingComponent } from './floor-by-building.component';

describe('FloorByBuildingComponent', () => {
  let component: FloorByBuildingComponent;
  let fixture: ComponentFixture<FloorByBuildingComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [FloorByBuildingComponent]
    });
    fixture = TestBed.createComponent(FloorByBuildingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
