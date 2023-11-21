import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BuildingsByFloorRangeComponent } from './buildings-by-floor-range.component';

describe('BuildingsByFloorRangeComponent', () => {
  let component: BuildingsByFloorRangeComponent;
  let fixture: ComponentFixture<BuildingsByFloorRangeComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [BuildingsByFloorRangeComponent]
    });
    fixture = TestBed.createComponent(BuildingsByFloorRangeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
