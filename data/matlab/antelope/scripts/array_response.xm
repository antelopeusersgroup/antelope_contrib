function z = array_response( varargin )

if (nargin == 4)
  points = varargin{2};
  range = varargin{3};
  frequency = varargin{4};
  stations = varargin{1};

elseif (nargin == 5)

  database = varargin{1};
  array = varargin{2};
  points = varargin{3};
  range = varargin{4};
  frequency = varargin{5};

  db = dbopen(database,'r');
  db = dblookup_table(db,'site') ;
  dbt = dblookup_table(db,'affiliation');
  db = dbjoin(db,dbt);
  db = dbsubset(db,['net == "' array '"']);
  [stations(:,1),stations(:,2)]=dbgetv(db,'dnorth','deast');

  else

  help array_response
  return

end

j = sqrt(-1);
d = (2*range)/points;                          % differential slowness

for y = 1:1:points
   for x = 1:1:points
      u = [(x*d-range) ; (y*d-range)];         % compose a slowness vector
      omega = 2*pi*frequency * stations * u;   % create a phase offset matrix
      phasor = sum(cos(omega)+j*sin(omega));   % add up the phasors
      z(x,y) = abs(phasor);                    % gain = abs(sum(phasors))
   end
end
